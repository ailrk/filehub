{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template.Common
  ( bootstrap
  , offline
  , withDefault
  , pathBreadcrumb
  , search
  , searchBar
  , controlPanel
  , icon
  , iconClass
  , open
  , bold
  , modal
  , fileNameElement
  , sizeElement
  , modifiedDateElement
  , sortIconName
  , sortIconMTime
  , sortIconSize
  , sortByText
  , viewId
  , sideBarId
  , controlPanelId
  , toolBarId
  , tableId
  , emptyDir
  , sideBar
  )
  where

import Filehub.Prelude
import Control.Monad.Reader (asks)
import Data.ClientPath (ClientPath(..), AbsPath (..), newAbsPath, Root(..))
import Data.ClientPath qualified as ClientPath
import Data.File (File(..), FileType(..), FileInfo, IsLink (..))
import Data.Foldable (Foldable(..))
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.String.Interpolate (iii)
import Filehub.Plugin.Types (StaticPlugin(..), ViewerEntry(..), UrlTemplate(..), EntryAction(..))
import Filehub.Locale ( Phrase(..), phrase )
import Filehub.Routes (Api (..))
import Filehub.Session (TargetView(..), ControlPanelState(..))
import Filehub.Template (Template, TemplateContext(..))
import Filehub.Size (toReadableSize)
import Filehub.Types (Display(..), SearchWord(..), SortFileBy (..))
import Lucid
import Lucid.Htmx (hxGet, HxTrigger (..), HxPost (..))
import Network.Mime.Extended (isMime)
import System.FilePath (splitPath, takeFileName)
import Data.DiskUsage (DiskUsage(..))
import Target.Home (Home)
import Target.Dummy (DummyTarget)
import Target.File (FileSys, Target(..))
import Target.S3 (S3, Target(..))
import Target.Types (targetHandler, handleTarget, AnyTarget)
import Target.Types qualified as Target
import Data.Ord (comparing, Down (..))
import Text.Fuzzy qualified as Fuzzy
import Filehub.Template.Htmx (asHtmx)


-- | The bootstrap page is used to detect the client's device  information.
--   Once we get what we need it will redirect to the real index.
bootstrap :: Html ()
bootstrap = do
  html_ do
    body_ $
      script_ [type_ "text/javascript"]
        ([iii|
            fetch('/init', {
              method: 'POST',
              headers: {
                "Content-Type": "application/x-www-form-urlencoded",
              },
              body: new URLSearchParams({
                res: window.innerWidth + 'x' + window.innerHeight
              })
            }).then(_ => { window.location.href = '/' })
        |] :: Text)


offline :: Html ()
offline = do
  html_ do
    body_
      "offline"


withDefault :: Display -> Text -> Html () -> Html ()
withDefault display background html = do
  script_ [ src_ "/static/htmx2.0.3.js" ] ("" :: Text)
  script_ [ src_ "/static/viewer.js", type_ "module" ] ("" :: Text)
  script_ [ src_ "/static/main.js", type_ "module" ] ("" :: Text)

  meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover" ]
  link_ [ rel_ "manifest", href_ "/manifest.json" ]
  link_ [ rel_ "icon", type_ "image/png", href_ "/favicon-96x96.png", sizes_ "96x96"]
  link_ [ rel_ "shortcut icon", href_ "/favicon.ico"]
  link_ [ rel_ "apple-touch-icon", sizes_ "180x180", href_ "/static/apple-touch-icon.png"]

  meta_ [ name_ "mobile-web-app-capable", content_ "yes" ]
  meta_ [ name_ "apple-mobile-web-app-title", content_ "FileHub"]
  meta_ [ name_ "apple-mobile-web-app-status-bar-style", content_ "black-translucent" ]
  meta_ [ name_ "theme-color", content_ background ]

  link_ [ rel_ "stylesheet", href_ "/static/boxicons2.1.4.css" ]
  link_ [ rel_ "stylesheet", href_ "/static/viewer.css" ]
  link_ [ rel_ "stylesheet", href_ "/static/reset.css" ]

  link_ [ rel_ "stylesheet", href_ "/theme.css" ]

  case display of
    Desktop   -> link_ [ rel_ "stylesheet", href_ "/static/style-desktop.css" ]
    Mobile    -> link_ [ rel_ "stylesheet", href_ "/static/style-mobile.css" ]
    NoDisplay -> link_ [ rel_ "stylesheet", href_ "/static/style-mobile.css" ]
  html
  div_ [ id_ "balloon-container"] mempty
  div_ [ id_ "paste-hl" ] mempty
  div_ [ id_ "global-spinner", class_ "htmx-indicator" ] do
    i_ [ class_ "bx bx-loader-alt" ] mempty


pathBreadcrumb :: Template (Html ())
pathBreadcrumb = do
  currentDir <- asks (.currentDir)
  root       <- asks (.root)
  pure do
    let breadcrumbItems =
          coerce currentDir
          & splitPath
          & scanl1 (++)
          & (\path -> if null path then ["/"] else path)
          & fmap newAbsPath
          & catMaybes
          & filter (\(AbsPath path) -> length (splitPath path) >= length (splitPath (coerce root)))
          & fmap (\path ->
            let clientPath@(ClientPath cp) = (ClientPath.toClientPath root path)
                mkLi                       = li_ (asHtmx @"cd" hxGet (Just clientPath)
                                              [ term "data-path" (txt_pack cp)
                                              , class_ "dir "
                                              , term "hx-indicator" "#global-spinner"
                                              ])
             in  (mkLi . toHtml . pathShow) (coerce path))
          & Seq.fromList
          & (\path -> Seq.adjust (`with` [class_ " active"]) (length path - 1) path)
          & sequence_
    div_ [ class_ "breadcrumb", id_ pathBreadcrumbId ] do
      ol_ breadcrumbItems
  where

    pathShow p =
      case Seq.fromList (splitPath p) of
        "/" :<| Seq.Empty  -> "Files"
        _ :|> l ->
          case Seq.fromList l of
            xs :|> '/' -> toList xs
            xs -> toList xs
        _ -> ""


search :: SearchWord -> [FileInfo] -> ([FileInfo] -> Template (Html ())) -> Template (Html ())
search (SearchWord searchWord) files table = do
  let scored = Fuzzy.filter searchWord files (\f -> txt_pack (takeFileName (coerce f.path))) False
      sorted = sortBy (comparing (Down . Fuzzy.score)) scored
  table (map Fuzzy.original sorted)


searchBar :: Template (Html ())
searchBar = do
  Phrase { search_as_you_type } <- phrase <$> asks (.locale)

  pure do
    div_ [ id_ searchBarId ] do
      input_ (asHtmx @"search" hxPost ()
        [ class_ "form-control "
        , type_ "input"
        , name_ "search"
        , placeholder_ search_as_you_type
        , hxTrigger @Text "input changed delay:200ms, search"
        ])


controlPanel
  :: Html () -> Html () -> Html () -> Html () ->
     Html () -> Html () -> Html () -> Html () -> Html () ->
     Html () -> Html () ->
     Maybe (Html()) -> Maybe (Html ()) -> Maybe (Html ()) ->
     Maybe (Html ()) ->
     Maybe (Html ()) ->
     Maybe (Html ()) ->
     Maybe (Html ()) ->
     Maybe (Html ())
  -> Template (Html ())
controlPanel
  localeBtn
  newFolderBtn
  newFileBtn
  uploadBtn
  copyBtn
  cutBtn
  pasteBtn
  deleteBtn
  cancelBtn
  themeBtn
  logoutBtn
  mSortBtn
  mToggleSidebarBtn
  mLayoutBtn
  mScroll2TopBtn
  mShareLinksBtn
  mHistoryBtn
  mSettingsBtn
  mShareSelectedBtn = do
    readOnly              <- asks (.readOnly)
    noLogin               <- asks (.noLogin)
    state                 <- asks (.state)
    TargetView { target } <- asks  (.currentTarget)
    Phrase { modal_readonly } <- phrase <$> asks (.locale)
    pure do
      case readOnly of
        True ->
          div_ [ id_ controlPanelId ] do
            fromMaybe mempty mToggleSidebarBtn
            fromMaybe mempty mLayoutBtn
            fromMaybe mempty mSortBtn
            themeBtn
            localeBtn
            when (not noLogin) logoutBtn
            fromMaybe mempty mSettingsBtn
            fromMaybe mempty mShareLinksBtn
            fromMaybe mempty mHistoryBtn
            fromMaybe mempty mScroll2TopBtn
            span_ [ class_ "btn-like field " ] do i_ [ class_ "bx bx-lock-alt" ] mempty >> span_ (toHtml modal_readonly)
        False ->
          case state of
            ControlPanelDefault ->
              div_ [ id_ controlPanelId ] do
                fromMaybe mempty mToggleSidebarBtn
                fromMaybe mempty mLayoutBtn
                fromMaybe mempty mSortBtn
                themeBtn
                localeBtn
                when (not noLogin) logoutBtn
                fromMaybe mempty mSettingsBtn
                sep
                newBtnGroup target
                fromMaybe mempty mShareLinksBtn
                fromMaybe mempty mHistoryBtn
                uploadBtn
                fromMaybe mempty mScroll2TopBtn
            ControlPanelSelecting ->
              div_ [ id_ controlPanelId ] do
                fromMaybe mempty mToggleSidebarBtn
                fromMaybe mempty mLayoutBtn
                fromMaybe mempty mSortBtn
                themeBtn
                localeBtn
                when (not noLogin) logoutBtn
                fromMaybe mempty mSettingsBtn
                sep
                newBtnGroup target
                fromMaybe mempty mShareLinksBtn
                fromMaybe mempty mHistoryBtn
                uploadBtn
                copyBtn
                cutBtn
                deleteBtn
                fromMaybe mempty mShareSelectedBtn
                div_ [id_ "plugin-toolbar-actions", term "data-cancel-hide" ""] mempty
                cancelBtn
                fromMaybe mempty mScroll2TopBtn
            ControlPanelCopied ->
              div_ [ id_ controlPanelId ] do
                fromMaybe mempty mToggleSidebarBtn
                fromMaybe mempty mLayoutBtn
                fromMaybe mempty mSortBtn
                themeBtn
                localeBtn
                when (not noLogin) logoutBtn
                fromMaybe mempty mSettingsBtn
                sep
                newBtnGroup target
                fromMaybe mempty mShareLinksBtn
                fromMaybe mempty mHistoryBtn
                uploadBtn
                pasteBtn
                cancelBtn
                fromMaybe mempty mScroll2TopBtn
  where
    sep = span_ [ style_ "display:inline-block; width: 20px"]  mempty
    newBtnGroup target = fromMaybe mempty do
      handleTarget target
        [ targetHandler @Home \_ -> mempty
        , targetHandler @S3 \_ -> do
            newFileBtn
        , targetHandler @FileSys \_ -> do
            newFolderBtn
            newFileBtn
        , targetHandler @DummyTarget \_ -> do
            newFolderBtn
            newFileBtn
        ]


iconClass :: FileInfo -> Text
iconClass file =
  case file.isLink of
    BrokenLink -> "bx bx-link-external"
    _          ->
      case file.content of
        Dir -> "bx bxs-folder"
        Regular
         | file.mimetype `isMime` "application/pdf"                       -> "bx bxs-file-pdf"
         | file.mimetype `isMime` "video" || file.mimetype `isMime` "mp4" -> "bx bxs-videos"
         | file.mimetype `isMime` "audio" || file.mimetype `isMime` "mp3" -> "bx bxs-music"
         | file.mimetype `isMime` "image"                                 -> "bx bx-image"
         | file.mimetype `isMime` "application/x-tar"
            || file.mimetype `isMime` "application/x-bzip-compressed-tar"
            || file.mimetype `isMime` "application/x-tgz"
            || file.mimetype `isMime` "application/x-bzip2"
            || file.mimetype `isMime` "application/x-zstd"
            || file.mimetype `isMime` "application/x-7z-compressed"
            || file.mimetype `isMime` "application/x-lzma"
            || file.mimetype `isMime` "application/x-lz"
            || file.mimetype `isMime` "application/zip"
            || file.mimetype `isMime` "application/gzip"
            || file.mimetype `isMime` "application/zstd"
            || file.mimetype `isMime` "application/vnd.rar"                -> "bx bxs-file-archive"
          | file.mimetype `isMime` "text"                                  -> "bx bxs-file"
          | otherwise                                                      -> "bx bxs-file-blank"

icon :: FileInfo -> Html ()
icon file = i_ [ class_ (iconClass file) ] mempty


open :: [StaticPlugin] -> Root -> ClientPath -> FileInfo -> [Attribute]
open plugins root clientPath file =
  case file.isLink of
    BrokenLink -> []
    _          ->
      case file.content of
        Dir        -> asHtmx @"cd" hxGet (Just clientPath) [term "hx-indicator" "#global-spinner"]
        Regular
          | Just entry <- findPluginViewer file.mimetype ->
               case entry.action of
                 UrlAction url ->
                   [ href_ (substituteViewerUrl url file root)
                   , target_ "_blank"
                   ]
                 ScriptAction script ->
                   [ term "data-action" "plugin-script"
                   , term "data-plugin-script" script
                   ]
          | file.mimetype `isMime` "application/pdf" ->
               [ term "data-action" "open-blank" ]
          | file.mimetype `isMime` "audio" ->
               [ term "data-action" "open-viewer" ]
          | file.mimetype `isMime` "video" ->
               [ term "data-action" "open-viewer" ]
          | file.mimetype `isMime` "image" ->
               [ term "data-action" "open-viewer" ]
          | file.mimetype `isMime` "text" ->
               asHtmx @"editorModal" hxGet (Just clientPath, Nothing) [ term "hx-indicator" "#global-spinner" ]
          | otherwise -> []
  where
    allViewers = concatMap (.viewer) plugins

    findPluginViewer mime =
      let -- skip url viewer entries that need {share_url} — those require async JS handling
          usable e = mime `isMime` e.mime && case e.action of
            UrlAction (UrlTemplate u) -> not ("{share_url}" `txt_isInfixOf` u)
            ScriptAction _            -> True
      in  find usable allViewers

    substituteViewerUrl (UrlTemplate tmpl) f r =
      let cp       = ClientPath.toClientPath r f.path
          filePath = txt_pack (show cp)
          fileName = txt_pack (takeFileName (coerce f.path))
          mimeType = txt_decode f.mimetype
      in  tmpl
            & txt_replace "{file_path}" filePath
            & txt_replace "{file_name}" fileName
            & txt_replace "{mime_type}" mimeType



bold :: Html () -> Html ()
bold t = span_ [ class_ "bold" ] t


modal :: [Attribute] -> Html () -> Html ()
modal attrs body = do
  div_ [ class_ "modal " ] do
    div_ [ class_ "modal-underlay " ] mempty
    div_ ([ class_ "modal-content " ] <> attrs) do
      body


fileNameElement :: FileInfo -> AnyTarget -> Bool -> Html ()
fileNameElement file target withIcon = do
  span_ do
    if withIcon
       then icon file
       else mempty
    name
    `with` [ title_ (txt_pack displayName) ]
  where
    name = span_ (toHtml displayName)
    displayName =
      fromMaybe "-" $ handleTarget target
        [ targetHandler @Home     \_ -> coerce takeFileName file.path
        , targetHandler @S3          \_ -> coerce file.path
        , targetHandler @FileSys     \_ -> coerce takeFileName file.path
        , targetHandler @DummyTarget \_ -> coerce takeFileName file.path
        ]


sizeElement :: FileInfo -> Html ()
sizeElement file =
  span_ (toHtml displaySize)
    `with` [ class_ "field file-meta "
           , title_ (txt_pack displaySize)
           ]
  where
    displaySize = toReadableSize (fromMaybe 0 file.size)


modifiedDateElement :: FileInfo -> Html ()
modifiedDateElement file =
  span_ (toHtml displayTime)
    `with` [ class_ "field file-meta "
           , title_ (txt_pack displayTime)
           ]
  where
    displayTime = maybe mempty (time_format "%Y/%m/%d") file.mtime


sortIconName :: SortFileBy -> Html ()
sortIconName = \case
  ByNameUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
  ByNameDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
  _          -> i_ [ class_ "bx bx-sort"] mempty


sortIconMTime :: SortFileBy -> Html ()
sortIconMTime = \case
  ByModifiedUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
  ByModifiedDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
  _              -> i_ [ class_ "bx bx-sort"] mempty


sortIconSize :: SortFileBy -> Html ()
sortIconSize = \case
  BySizeUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
  BySizeDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
  _          -> i_ [ class_ "bx bx-sort"] mempty


sortByText :: SortFileBy -> Text
sortByText = \case
  ByNameUp       -> "nameUp"
  ByNameDown     -> "nameDown"
  ByModifiedUp   -> "modifiedUp"
  ByModifiedDown -> "modifiedDown"
  BySizeUp       -> "sizeUp"
  BySizeDown     -> "sizeDown"


------------------------------------
-- component ids
------------------------------------


viewId :: Text
viewId = "view"

pathBreadcrumbId :: Text
pathBreadcrumbId = "path-breadcrumb"

tableId :: Text
tableId = "table"

searchBarId :: Text
searchBarId = "search-bar"

controlPanelId :: Text
controlPanelId = "control-panel"

sideBarId :: Text
sideBarId = "side-bar"

toolBarId :: Text
toolBarId = "tool-bar"


emptyDir :: Template (Html ())
emptyDir = do
  Phrase { folder_empty } <- phrase <$> asks (.locale)
  pure do
    div_ [ class_ "empty-dir" ] do
      i_ [ class_ "bx bx-folder-open" ] mempty
      span_ (toHtml folder_empty)


sideBar :: [(AnyTarget, Int, Maybe DiskUsage)] -> TargetView -> Template (Html ())
sideBar targets (TargetView currentTarget _) = do
  p <- phrase <$> asks (.locale)
  pure do
    div_ [ id_ sideBarId ] do
      traverse_ (targetTab p) targets
  where
    targetTab :: Phrase -> (AnyTarget, Int, Maybe DiskUsage) -> Html ()
    targetTab Phrase { target_filesystem, target_s3 } (target, selectedCount, mUsage) = do
      div_ (asHtmx @"changeTarget" hxGet (Just (Target.getTargetId target))
           [ class_ "target-tab", term "hx-indicator" "#global-spinner" ]) do
        div_ [ class_ "target-tab-row1" ] do
          span_ [ class_ "field" ] do
            fromMaybe mempty $ handleTarget target
              [ targetHandler @Home     \_ -> i_ [ class_ "bx bxs-home"  ] mempty
              , targetHandler @S3          \_ -> i_ [ class_ "bx bxs-cube"  ] mempty
              , targetHandler @FileSys     \_ -> i_ [ class_ "bx bx-folder" ] mempty
              , targetHandler @DummyTarget \_ -> i_ [ class_ "bx bxs-cube"  ] mempty
              ]
            fromMaybe mempty $ handleTarget target
              [ targetHandler @Home \_ -> span_ "Home"
              , targetHandler @S3      \(S3Backend { bucket }) -> span_ [iii| /#{bucket} |]
              , targetHandler @FileSys \(FileBackend { root = Root (AbsPath root) }) -> span_ [iii| /#{takeFileName root} |]
              , targetHandler @DummyTarget \_ -> span_ "dummy"
              ]
          when (selectedCount > 0) do
            div_ [ class_ "target-tab-selected-counter" ] do
              (toHtml . txt_pack . show) selectedCount

        case mUsage of
          Nothing -> mempty
          Just DiskUsage { total, used } ->
            if total > 0
              then
                let pct     = (used * 100) `div` total :: Integer
                    urgency = if pct > 90 then " urgent" else if pct > 70 then " warn" else "" :: Text
                in div_ [ class_ "sidebar-usage" ] do
                     div_ [ class_ "sidebar-usage-track" ] do
                       div_ [ class_ [iii|sidebar-usage-bar#{urgency}|]
                            , style_ [iii|width:#{pct}%|]
                            ] mempty
                     span_ [ class_ "sidebar-usage-label" ] do
                       toHtml (formatBytes used <> " / " <> formatBytes total)
              else
                div_ [ class_ "sidebar-usage" ] do
                  span_ [ class_ "sidebar-usage-label" ] do
                    toHtml (formatBytes used <> " used")

      `with` targetAttr target
      `with` targetDataAttrs target
      where
        targetAttr t = [class_ " current-target" | Target.getTargetId currentTarget == Target.getTargetId t]
        targetDataAttrs t =
          fromMaybe [] $ handleTarget t
            [ targetHandler @Home \_ ->
                [ term "data-target-type" "home" ]
            , targetHandler @S3 \(S3Backend { bucket }) ->
                [ term "data-target-type" "s3"
                , term "data-target-name" bucket
                , term "data-target-info" [iii| [#{target_s3}] #{bucket} |]
                ]
            , targetHandler @FileSys \(FileBackend { root = Root (AbsPath root) }) ->
                [ term "data-target-type" "fs"
                , term "data-target-info" [iii| [#{target_filesystem}] #{takeFileName root} |]
                ]
            ]

    formatBytes :: Integer -> Text
    formatBytes b
      | b >= gb   = fmt (b `div` gb)   "GB"
      | b >= mb   = fmt (b `div` mb)   "MB"
      | b >= kb   = fmt (b `div` kb)   "KB"
      | otherwise = txt_pack (show b) <> " B"
      where
        kb  = 1024
        mb  = 1024 * kb
        gb  = 1024 * mb
        fmt n unit = txt_pack (show n) <> " " <> unit
