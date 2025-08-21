module Filehub.Template.Internal where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as LText
import Filehub.Types ( ClientPath(..), Display(..), ControlPanelState (..), File(..), FileContent (..), OpenTarget (..))
import Filehub.ClientPath qualified as ClientPath
import Filehub.Links ( apiLinks, linkToText )
import Filehub.Routes (Api(..))
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Foldable (Foldable(..))
import System.FilePath (splitPath)
import Filehub.Mime (isMime)
import Control.Monad (when)


withDefault :: Display -> Text -> Html () -> Html ()
withDefault display background html = do

  script_ [ src_ "/static/_hyperscript0.9.13.js" ] ("" :: Text)
  script_ [ src_ "/static/htmx2.0.3.js" ] ("" :: Text)
  script_ [ src_ "/static/viewer.js", type_ "module" ] ("" :: Text)
  script_ [ src_ "/static/ui.js", type_ "module" ] ("" :: Text)

  meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover" ]
  link_ [ rel_ "stylesheet", href_ "/static/boxicons2.1.4.css" ]
  link_ [ rel_ "stylesheet", href_ "/static/viewer.css" ]
  link_ [ rel_ "manifest", href_ "/manifest.json" ]
  link_ [ rel_ "icon", type_ "image/png", href_ "/favicon-96x96.png", sizes_ "96x96"]
  link_ [ rel_ "icon", type_ "image/svg+xml", href_ "/favicon.svg"]
  link_ [ rel_ "shortcut icon", href_ "/favicon.ico"]
  link_ [ rel_ "apple-touch-icon", sizes_ "180x180", href_ "/apple-touch-icon.png"]
  link_ [ rel_ "stylesheet", href_ "/theme.css" ]

  meta_ [ name_ "mobile-web-app-capable", content_ "yes" ]
  meta_ [ name_ "apple-mobile-web-app-title", content_ "FileHub"]
  meta_ [ name_ "apple-mobile-web-app-status-bar-style", content_ "black-translucent" ]
  meta_ [ name_ "apple-mobile-web-app-title", content_ "Filehub" ]
  meta_ [ name_ "theme-color", content_ background ]

  case display of
    Desktop -> link_ [ rel_ "stylesheet", href_ "/static/desktop.css" ]
    Mobile -> link_ [ rel_ "stylesheet", href_ "/static/mobile.css" ]
    NoDisplay -> link_ [ rel_ "stylesheet", href_ "/static/mobile.css" ]
  html


pathBreadcrumb :: FilePath -> FilePath -> Html ()
pathBreadcrumb currentDir root = do
  let breadcrumbItems =
        currentDir
        & splitPath
        & scanl1 (++)
        & (\path-> if null path then ["/"] else path)
        & filter (\path -> length (splitPath path) >= length (splitPath root))
        & fmap (\path ->
          let clientPath@(ClientPath cp) =  (ClientPath.toClientPath root path)
           in li_ [ term "hx-get" $ linkToText (apiLinks.cd (Just clientPath))
                  , term "hx-target" ("#" <> viewId)
                  , term "hx-swap" "outerHTML"
                  , term "data-path" (Text.pack cp)
                  , class_ "dir "
                  ] . toHtml . pathShow  $ path)
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


searchBar :: Html ()
searchBar = do
  div_ [ id_ searchBarId ] do
    input_ [ class_ "form-control "
           , type_ "input"
           , name_ "search"
           , placeholder_ "Search as you type"
           , term "hx-post" $ linkToText apiLinks.search
           , term "hx-trigger" "input changed delay:200ms, search"
           , term "hx-target" "#table"
           , term "hx-swap" "outerHTML"
           ]


controlPanel
  :: Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Maybe (Html()) -> Maybe (Html ())
  -> Bool -> Bool -> ControlPanelState -> Html ()
controlPanel
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
  mScroll2TopBtn
  readOnly noLogin state = do
  case readOnly of
    True ->
      div_ [ id_ controlPanelId ] do
        maybe mempty id mLayoutBtn
        themeBtn
        maybe mempty id mScroll2TopBtn
        span_ [ class_ "btn-like field " ] do
          i_ [ class_ "bx bx-lock-alt" ] mempty
          span_ "Read-only"
    False ->
      case state of
        ControlPanelDefault ->
          div_ [ id_ controlPanelId ] do
            maybe mempty id mLayoutBtn
            themeBtn
            when (not noLogin) logoutBtn
            sep
            newFolderBtn
            newFileBtn
            uploadBtn
            maybe mempty id mScroll2TopBtn
        ControlPanelSelecting ->
          div_ [ id_ controlPanelId ] do
            maybe mempty id mLayoutBtn
            themeBtn
            when (not noLogin) logoutBtn
            sep
            newFolderBtn
            newFileBtn
            uploadBtn
            copyBtn
            deleteBtn
            cancelBtn
            maybe mempty id mScroll2TopBtn
        ControlPanelCopied ->
          div_ [ id_ controlPanelId ] do
            maybe mempty id mLayoutBtn
            themeBtn
            when (not noLogin) logoutBtn
            sep
            newFolderBtn
            newFileBtn
            uploadBtn
            pasteBtn
            cancelBtn
            maybe mempty id mScroll2TopBtn
  where
    sep = span_ [ style_ "display:inline-block; width: 20px"]  mempty


login :: Html ()
login = do
  meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover" ]
  link_ [ rel_ "stylesheet", href_ "/static/login.css" ]
  link_ [ rel_ "icon", type_ "image/png", href_ "/favicon-96x96.png", sizes_ "96x96"]
  link_ [ rel_ "icon", type_ "image/svg+xml", href_ "/favicon.svg"]
  link_ [ rel_ "shortcut icon", href_ "/favicon.ico"]
  link_ [ rel_ "stylesheet", href_ "/theme.css" ]
  script_ [ src_ "/static/htmx2.0.3.js" ] ("" :: Text)

  div_ [ id_ "login"  ] $ do
    form_ [ term "hx-post" "/login"
          , term "hx-target" "#login-error"
          , term "hx-swap" "outerHTML"
          , autocomplete_ "off"
          ] $ do
      div_ $ do
        input_ [ type_ "text"
               , id_ "username"
               , name_ "username"
               , placeholder_ "Username"
               , autocomplete_ "new-password"
               ]
      div_ $ do
        input_ [ type_ "password"
               , id_ "password"
               , name_ "password"
               , placeholder_ "Password"
               , autocomplete_ "off"
               ]
      div_ [ id_ "login-error"
           , term "hx-target" "this"
           , term "hx-swap" "outerHTML"
           ] mempty
      div_ $
        button_ "Login"


loginFailed :: Html ()
loginFailed =
      div_ [ id_ "login-error"
           , class_ "show fade-in "
           , term "hx-target" "this"
           , term "hx-swap" "outerHTML"
           ] do
    span_ "Incorrect username or password"


icon :: File -> Html ()
icon file =
  case file.content of
    Dir _ -> i_ [ class_ "bx bxs-folder "] mempty
    Content
      | file.mimetype `isMime` "application/pdf" -> i_ [ class_ "bx bxs-file-pdf"] mempty
      | file.mimetype `isMime` "video" || file.mimetype `isMime` "mp4" -> i_ [ class_ "bx bxs-videos"] mempty
      | file.mimetype `isMime` "audio" || file.mimetype `isMime` "mp3" -> i_ [ class_ "bx bxs-music"] mempty
      | file.mimetype `isMime` "image" -> i_ [ class_ "bx bx-image"] mempty
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
        || file.mimetype `isMime` "application/vnd.rar" -> i_ [ class_ "bx bxs-file-archive"] mempty
      | file.mimetype `isMime` "text" -> i_ [ class_ "bx bxs-file"] mempty
      | otherwise -> i_ [ class_ "bx bxs-file-blank "] mempty


open :: FilePath -> File -> [Attribute]
open root file = do
  let clientPath = ClientPath.toClientPath root file.path
  case file.content of
    Dir _ ->
        [ term "hx-get" $ linkToText (apiLinks.cd (Just clientPath))
        , term "hx-target" ("#" <> viewId)
        , term "hx-swap" "outerHTML"
        ]
    Content
      | file.mimetype `isMime` "application/pdf" ->
          [ term "hx-get" $ linkToText (apiLinks.open (Just OpenDOMBlank) (Just clientPath))
          , term "hx-target" "this"
          , term "hx-swap" "none"
          ]
      | file.mimetype `isMime` "audio" ->
          [ term "hx-get" $ linkToText (apiLinks.open (Just OpenViewer) (Just clientPath))
          , term "hx-target" "this"
          , term "hx-swap" "none"
          ]
      | file.mimetype `isMime` "video" ->
          [ term "hx-get" $ linkToText (apiLinks.open (Just OpenViewer) (Just clientPath))
          , term "hx-target" "this"
          , term "hx-swap" "none"
          ]
      | file.mimetype `isMime` "image" ->
          [ term "hx-get" $ linkToText (apiLinks.open (Just OpenViewer) (Just clientPath))
          , term "hx-target" "this"
          , term "hx-swap" "none"
          ]
      | file.mimetype `isMime` "text" ->
          [ term "hx-get" $ linkToText (apiLinks.editorModal (Just clientPath))
          , term "hx-target" "#index"
          , term "hx-swap" "beforeend"
          ]
      | otherwise ->
          [ term "hx-get" $ linkToText (apiLinks.editorModal (Just clientPath))
          , term "hx-target" "#index"
          , term "hx-swap" "beforeend"
          ]


bold :: Html () -> Html ()
bold t = span_ [ class_ "bold" ] t


toClientPath :: FilePath -> FilePath -> Text
toClientPath root p = Text.pack . (.unClientPath) $ ClientPath.toClientPath root p


toHxVals :: [Pair] -> Text
toHxVals xs = (xs & Aeson.object & Aeson.encode & LText.decodeUtf8) ^. strict


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
