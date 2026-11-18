{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
module Filehub.Template.Shared
  ( bootstrap
  , offline
  , withDefault
  , pathBreadcrumb
  , search
  , searchBar
  , controlPanel
  , icon
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
  )
  where

import Control.Monad (when)
import Control.Monad.Reader (asks)
import Data.ClientPath (ClientPath(..), AbsPath (..), newAbsPath, Root)
import Data.ClientPath qualified as ClientPath
import Data.Coerce (coerce)
import Data.File (File(..), FileType(..), FileInfo, IsLink (..))
import Data.Foldable (Foldable(..))
import Data.Function ((&))
import Data.Maybe (fromMaybe, catMaybes)
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.String.Interpolate (iii)
import Data.Text (Text)
import Data.Text qualified as Text
import Filehub.Routes.Links (apiLinks)
import Filehub.Locale ( Phrase(..), phrase )
import Filehub.Routes (Api (..))
import Filehub.Session (TargetView(..))
import Filehub.Session.Types (ControlPanelState(..))
import Filehub.Template (Template, TemplateContext(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Filehub.Size (toReadableSize)
import Filehub.Types (Display(..), OpenTarget(..), SearchWord(..), SortFileBy (..))
import Lucid
import Lucid.Htmx (hxGet, HxTrigger (..), HxPost (..))
import Network.Mime.Extended (isMime)
import System.FilePath (splitPath, takeFileName)
import Target.Dummy (DummyTarget)
import Target.File (FileSys)
import Target.S3 (S3)
import Target.Types (targetHandler, handleTarget, AnyTarget)
import Data.List (sortBy)
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
                                              [ term "data-path" (Text.pack cp)
                                              , class_ "dir "
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
  let scored = Fuzzy.filter searchWord files "" "" (\f -> Text.pack (takeFileName (coerce f.path))) False
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
     Html () -> Html () -> Html () -> Html () ->
     Html () -> Html () ->
     Maybe (Html()) -> Maybe (Html ()) -> Maybe (Html ()) ->
     Maybe (Html ()) ->
     Maybe (Html ())
  -> Template (Html ())
controlPanel
  localeBtn
  newFolderBtn
  newFileBtn
  uploadBtn
  copyBtn
  pasteBtn
  deleteBtn
  cancelBtn
  themeBtn
  logoutBtn
  mSortBtn
  mToggleSidebarBtn
  mLayoutBtn
  mScroll2TopBtn
  mShareLinksBtn = do
    readOnly              <- asks (.readOnly)
    noLogin               <- asks (.noLogin)
    state                 <- asks (.state)
    TargetView { target } <- asks  (.currentTarget)
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
            fromMaybe mempty mShareLinksBtn
            fromMaybe mempty mScroll2TopBtn
            span_ [ class_ "btn-like field " ] do i_ [ class_ "bx bx-lock-alt" ] mempty >> span_ "Read-only"
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
                sep
                newBtnGroup target
                fromMaybe mempty mShareLinksBtn
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
                sep
                newBtnGroup target
                fromMaybe mempty mShareLinksBtn
                uploadBtn
                copyBtn
                deleteBtn
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
                sep
                newBtnGroup target
                fromMaybe mempty mShareLinksBtn
                uploadBtn
                pasteBtn
                cancelBtn
                fromMaybe mempty mScroll2TopBtn
  where
    sep = span_ [ style_ "display:inline-block; width: 20px"]  mempty
    newBtnGroup target = fromMaybe mempty do
      handleTarget target
        [ targetHandler @S3 \_ -> do
            newFileBtn
        , targetHandler @FileSys \_ -> do
            newFolderBtn
            newFileBtn
        , targetHandler @DummyTarget \_ -> do
            newFolderBtn
            newFileBtn
        ]


icon :: FileInfo -> Html ()
icon file =
  case file.isLink of
    BrokenLink -> i_ [ class_ "bx bx-link-external .urgent "] mempty
    _          ->
      case file.content of
        Dir -> i_ [ class_ "bx bxs-folder "] mempty
        Regular
         | file.mimetype `isMime` "application/pdf"                       -> i_ [ class_ "bx bxs-file-pdf"] mempty
         | file.mimetype `isMime` "video" || file.mimetype `isMime` "mp4" -> i_ [ class_ "bx bxs-videos"] mempty
         | file.mimetype `isMime` "audio" || file.mimetype `isMime` "mp3" -> i_ [ class_ "bx bxs-music"] mempty
         | file.mimetype `isMime` "image"                                 -> i_ [ class_ "bx bx-image"] mempty
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
            || file.mimetype `isMime` "application/vnd.rar"                -> i_ [ class_ "bx bxs-file-archive"] mempty
          | file.mimetype `isMime` "text"                                  -> i_ [ class_ "bx bxs-file"] mempty
          | otherwise                                                      -> i_ [ class_ "bx bxs-file-blank "] mempty


open :: Root -> FileInfo -> [Attribute]
open root file = do
  case file.isLink of
    BrokenLink -> []
    _          ->
      case file.content of
        Dir        -> asHtmx @"cd" hxGet (Just clientPath) []
        Regular
          | file.mimetype `isMime` "application/pdf" ->
               [ hxGet (apiLinks.open (Just OpenDOMBlank) (Just clientPath))
               , term "hx-swap" "none"
               , term "hx-indicator" "#global-spinner"
               ]
          | file.mimetype `isMime` "audio" ->
               [ hxGet (apiLinks.open (Just OpenViewer) (Just clientPath))
               , term "hx-swap" "none"
               , term "hx-indicator" "#global-spinner"
               ]
          | file.mimetype `isMime` "video" ->
               [ hxGet (apiLinks.open (Just OpenViewer) (Just clientPath))
               , term "hx-swap" "none"
               , term "hx-indicator" "#global-spinner"
               ]
          | file.mimetype `isMime` "image" ->
               [ hxGet (apiLinks.open (Just OpenViewer) (Just clientPath))
               , term "hx-swap" "none"
               , term "hx-indicator" "#global-spinner"
               ]
          | file.mimetype `isMime` "text" ->
               asHtmx @"editorModal" hxGet (Just clientPath) [ term "hx-indicator" "#global-spinner" ]
          | otherwise ->
               asHtmx @"editorModal" hxGet (Just clientPath) [ term "hx-indicator" "#global-spinner" ]
  where
    clientPath = ClientPath.toClientPath root file.path


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
    `with` [ title_ (Text.pack displayName) ]
  where
    name = span_ (toHtml displayName)
    displayName =
      fromMaybe "-" $ handleTarget target
        [ targetHandler @S3          \_ -> coerce file.path
        , targetHandler @FileSys     \_ -> coerce takeFileName file.path
        , targetHandler @DummyTarget \_ -> coerce takeFileName file.path
        ]


sizeElement :: FileInfo -> Html ()
sizeElement file =
  span_ (toHtml displaySize)
    `with` [ class_ "field file-meta "
           , title_ (Text.pack displaySize)
           ]
  where
    displaySize = toReadableSize (fromMaybe 0 file.size)


modifiedDateElement :: FileInfo -> Html ()
modifiedDateElement file =
  span_ (toHtml displayTime)
    `with` [ class_ "field file-meta "
           , title_ (Text.pack displayTime)
           ]
  where
    displayTime = maybe mempty (formatTime defaultTimeLocale "%Y/%m/%d") file.mtime


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
