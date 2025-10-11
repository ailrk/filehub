{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template
  ( TemplateContext(..)
  , Template
  , runTemplate
  , makeTemplateContext
  , bootstrap
  , offline
  , withDefault
  , pathBreadcrumb
  , search
  , searchBar
  , controlPanel
  , icon
  , open
  , bold
  , viewId
  , sideBarId
  , controlPanelId
  , toolBarId
  , tableId
  )
  where

import Lucid
import Data.Text (Text)
import Data.String.Interpolate (iii)
import Control.Monad (when)
import Data.ClientPath (ClientPath(..))
import Data.ClientPath qualified as ClientPath
import Data.File (File(..), FileType(..), FileInfo)
import Data.Foldable (Foldable(..))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Effectful.Reader.Dynamic ( asks, ask, Reader, runReader )
import Filehub.Links (linkToText, apiLinks)
import Filehub.Locale ( Phrase(..), phrase, Locale )
import Filehub.Routes (Api (..))
import Filehub.Sort ( sortFiles, SortFileBy )
-- import Filehub.Template.Internal
import Filehub.Types
    ( Display(..),
      ControlPanelState(..),
      OpenTarget(..),
      SearchWord(..),
      Env,
      Layout,
      Selected )
import Lens.Micro
import Lens.Micro.Platform ()
import Network.Mime.Extended (isMime)
import System.FilePath (splitPath)
import Target.File (FileSys)
import Target.S3 (S3)
import Target.Types (targetHandler, handleTarget)
import Text.Fuzzy (simpleFilter)
import Filehub.Session (TargetView(..), SessionId)
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Session
import Filehub.Theme (Theme)
import Effectful (Eff, runPureEff)
import Filehub.Auth.Simple (SimpleAuthUserDB)
import Filehub.Auth.OIDC (OIDCAuthProviders)
import Filehub.Monad (Filehub)
import Filehub.Env qualified as Env
import Target.Dummy (DummyTarget)


-- | A Template context type that capture all useful information to render
-- a HTML.
--
-- == On laziness
--   All fields are lazy. Most of these fields are from a session which requires a
--   Map lookup with a sessionId, if the record is strict, we need to perform lookup for
--   every field even when we don't need them. Being lazy means we pay exactly what we
--   need, which is pretty good.
data TemplateContext = TemplateContext
  { readOnly          :: ~Bool
  , noLogin           :: ~Bool
  , display           :: ~Display
  , layout            :: ~Layout
  , theme             :: ~Theme
  , selected          :: ~Selected
  , sortedBy          :: ~SortFileBy
  , locale            :: ~Locale
  , state             :: ~ControlPanelState
  , currentDir        :: ~FilePath
  , currentTarget     :: ~TargetView
  , root              :: ~FilePath
  , simpleAuthUserDB  :: ~SimpleAuthUserDB
  , oidcAuthProviders :: ~OIDCAuthProviders
  }


runTemplate :: TemplateContext -> Template a -> a
runTemplate ctx = runPureEff . runReader ctx


type Template =  Eff '[Reader TemplateContext]


makeTemplateContext :: SessionId -> Filehub TemplateContext
makeTemplateContext sessionId = do
  theme             <- Session.getSessionTheme sessionId
  layout            <- Session.getLayout sessionId
  readOnly          <- asks @Env (.readOnly)
  noLogin           <- Env.hasNoLogin <$> ask @Env
  display           <- Session.getDisplay sessionId
  state             <- Session.getControlPanelState sessionId
  root              <- Session.getRoot sessionId
  sortedBy          <- Session.getSortFileBy sessionId
  selected          <- Session.getSelected sessionId
  currentDir        <- Session.getCurrentDir sessionId
  currentTarget     <- Session.currentTarget sessionId
  locale            <- Session.getSessionLocale sessionId
  simpleAuthUserDB  <- asks @Env (.simpleAuthUserDB)
  oidcAuthProviders <- asks @Env (.oidcAuthProviders)
  pure TemplateContext
    { readOnly           = readOnly
    , noLogin            = noLogin
    , display            = display
    , layout             = layout
    , theme              = theme
    , sortedBy           = sortedBy
    , selected           = selected
    , state              = state
    , root               = root
    , locale             = locale
    , currentDir         = currentDir
    , currentTarget      = currentTarget
    , simpleAuthUserDB   = simpleAuthUserDB
    , oidcAuthProviders  = oidcAuthProviders
    }


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
  script_ [ src_ "/static/_hyperscript0.9.13.js" ] ("" :: Text)
  script_ [ src_ "/static/htmx2.0.3.js" ] ("" :: Text)
  script_ [ src_ "/static/viewer.js", type_ "module" ] ("" :: Text)
  script_ [ src_ "/static/main.js", type_ "module" ] ("" :: Text)

  meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover" ]
  link_ [ rel_ "manifest", href_ "/manifest.json" ]
  link_ [ rel_ "icon", type_ "image/png", href_ "/favicon-96x96.png", sizes_ "96x96"]
  link_ [ rel_ "icon", type_ "image/svg+xml", href_ "/favicon.svg"]
  link_ [ rel_ "shortcut icon", href_ "/favicon.ico"]
  link_ [ rel_ "apple-touch-icon", sizes_ "180x180", href_ "/apple-touch-icon.png"]

  meta_ [ name_ "mobile-web-app-capable", content_ "yes" ]
  meta_ [ name_ "apple-mobile-web-app-title", content_ "FileHub"]
  meta_ [ name_ "apple-mobile-web-app-status-bar-style", content_ "black-translucent" ]
  meta_ [ name_ "apple-mobile-web-app-title", content_ "Filehub" ]
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


pathBreadcrumb :: Template (Html ())
pathBreadcrumb = do
  currentDir <- asks @TemplateContext (.currentDir)
  root <- asks @TemplateContext (.root)
  pure do
    let breadcrumbItems =
          currentDir
          & splitPath
          & scanl1 (++)
          & (\path-> if null path then ["/"] else path)
          & filter (\path -> length (splitPath path) >= length (splitPath root))
          & fmap (\path ->
            let clientPath@(ClientPath cp) = (ClientPath.toClientPath root path)
                mkLi                       = li_ [ term "hx-get" (linkToText (apiLinks.cd (Just clientPath)))
                                                 , term "hx-target" ("#" <> viewId)
                                                 , term "hx-swap" "outerHTML"
                                                 , term "data-path" (Text.pack cp)
                                                 , class_ "dir "
                                                 ]
             in  (mkLi . toHtml . pathShow) path)
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
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  order <- asks @TemplateContext (.sortedBy)
  table (sortFiles order filteredFiles)


searchBar :: Template (Html ())
searchBar = do
  Phrase { search_as_you_type } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    div_ [ id_ searchBarId ] do
      input_ [ class_ "form-control "
             , type_ "input"
             , name_ "search"
             , placeholder_ search_as_you_type
             , term "hx-post" (linkToText apiLinks.search)
             , term "hx-trigger" "input changed delay:200ms, search"
             , term "hx-target" "#table"
             , term "hx-swap" "outerHTML"
             ]


controlPanel
  :: Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Maybe (Html()) -> Maybe (Html ())
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
  mLayoutBtn
  mScroll2TopBtn = do
    readOnly <- asks @TemplateContext (.readOnly)
    noLogin <- asks @TemplateContext (.noLogin)
    state <- asks @TemplateContext (.state)
    TargetView { target } <- asks  @TemplateContext (.currentTarget)
    pure do
      case readOnly of
        True ->
          div_ [ id_ controlPanelId ] do
            fromMaybe mempty mLayoutBtn
            themeBtn
            localeBtn
            when (not noLogin) logoutBtn
            fromMaybe mempty mScroll2TopBtn
            span_ [ class_ "btn-like field " ] do i_ [ class_ "bx bx-lock-alt" ] mempty >> span_ "Read-only"
        False ->
          case state of
            ControlPanelDefault ->
              div_ [ id_ controlPanelId ] do
                fromMaybe mempty mLayoutBtn
                themeBtn
                localeBtn
                when (not noLogin) logoutBtn
                sep
                newBtnGroup target
                uploadBtn
                fromMaybe mempty mScroll2TopBtn
            ControlPanelSelecting ->
              div_ [ id_ controlPanelId ] do
                fromMaybe mempty mLayoutBtn
                themeBtn
                localeBtn
                when (not noLogin) logoutBtn
                sep
                newBtnGroup target
                uploadBtn
                copyBtn
                deleteBtn
                cancelBtn
                fromMaybe mempty mScroll2TopBtn
            ControlPanelCopied ->
              div_ [ id_ controlPanelId ] do
                fromMaybe mempty mLayoutBtn
                themeBtn
                localeBtn
                when (not noLogin) logoutBtn
                sep
                newBtnGroup target
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
      || file.mimetype `isMime` "application/vnd.rar"                  -> i_ [ class_ "bx bxs-file-archive"] mempty
      | file.mimetype `isMime` "text"                                  -> i_ [ class_ "bx bxs-file"] mempty
      | otherwise                                                      -> i_ [ class_ "bx bxs-file-blank "] mempty


open :: FilePath -> FileInfo -> [Attribute]
open root file = do
  let clientPath = ClientPath.toClientPath root file.path
  case file.content of
    Dir ->
        [ term "hx-get" (linkToText (apiLinks.cd (Just clientPath)))
        , term "hx-target" ("#" <> viewId)
        , term "hx-swap" "outerHTML"
        ]
    Regular
      | file.mimetype `isMime` "application/pdf" ->
          [ term "hx-get" (linkToText (apiLinks.open (Just OpenDOMBlank) (Just clientPath)))
          , term "hx-target" "this"
          , term "hx-swap" "none"
          ]
      | file.mimetype `isMime` "audio" ->
          [ term "hx-get" (linkToText (apiLinks.open (Just OpenViewer) (Just clientPath)))
          , term "hx-target" "this"
          , term "hx-swap" "none"
          ]
      | file.mimetype `isMime` "video" ->
          [ term "hx-get" (linkToText (apiLinks.open (Just OpenViewer) (Just clientPath)))
          , term "hx-target" "this"
          , term "hx-swap" "none"
          ]
      | file.mimetype `isMime` "image" ->
          [ term "hx-get" (linkToText (apiLinks.open (Just OpenViewer) (Just clientPath)))
          , term "hx-target" "this"
          , term "hx-swap" "none"
          ]
      | file.mimetype `isMime` "text" ->
          [ term "hx-get" (linkToText (apiLinks.editorModal (Just clientPath)))
          , term "hx-target" "#index"
          , term "hx-swap" "beforeend"
          ]
      | otherwise ->
          [ term "hx-get" (linkToText (apiLinks.editorModal (Just clientPath)))
          , term "hx-target" "#index"
          , term "hx-swap" "beforeend"
          ]


bold :: Html () -> Html ()
bold t = span_ [ class_ "bold" ] t


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
