module Filehub.Template.Internal where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as LText
import Filehub.Types ( ClientPath(..), Display(..), ControlPanelState (..))
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
import Data.Bifunctor (Bifunctor(..))


withDefault :: Display -> Text -> Html () -> Html ()
withDefault display background html = do

  script_ [ src_ "https://unpkg.com/hyperscript.org@0.9.13" ] ("" :: Text)
  script_ [ src_ "https://unpkg.com/htmx.org@2.0.3" ] ("" :: Text)
  script_ [ src_ "/static/filehub/viewer.js", type_ "module" ] ("" :: Text)
  script_ [ src_ "/static/filehub/ui.js", type_ "module" ] ("" :: Text)

  meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover" ]
  link_ [ rel_ "stylesheet", href_ "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css" ]
  link_ [ rel_ "stylesheet", href_ "/static/filehub/viewer.css" ]
  link_ [ rel_ "manifest", href_ "/manifest.json" ]
  link_ [rel_ "icon", type_ "image/png", href_ "/favicon-96x96.png", sizes_ "96x96"]
  link_ [rel_ "icon", type_ "image/svg+xml", href_ "/favicon.svg"]
  link_ [rel_ "shortcut icon", href_ "/favicon.ico"]
  link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/apple-touch-icon.png"]

  meta_ [ name_ "mobile-web-app-capable", content_ "yes" ]
  meta_ [ name_ "apple-mobile-web-app-title", content_ "FileHub"]
  meta_ [ name_ "apple-mobile-web-app-status-bar-style", content_ "black-translucent" ]
  meta_ [ name_ "apple-mobile-web-app-title", content_ "Filehub" ]
  meta_ [ name_ "theme-color", content_ background ]

  case display of
    Desktop -> link_ [ rel_ "stylesheet", href_ "/static/filehub/desktop.css" ]
    Mobile -> link_ [ rel_ "stylesheet", href_ "/static/filehub/mobile.css" ]
    NoDisplay -> link_ [ rel_ "stylesheet", href_ "/static/filehub/mobile.css" ]
  html


pathBreadcrumb :: FilePath -> FilePath -> Html ()
pathBreadcrumb currentDir root = do
  let afterRoot path = length (splitPath path) >= length (splitPath root)
      breadcrumbItems =
        currentDir
        & splitPath
        & scanl1 (++)
        & (\xs -> if null xs then ["/"] else xs)
        & filter afterRoot
        & fmap toAttrsTuple
        & Seq.fromList
        & adjustLast (addAttr " active")
        & fmap toLi
        & sequence_
  div_ [ class_ "breadcrumb", id_ pathBreadcrumbId ] do
    ol_ breadcrumbItems
  where
    adjustLast f xs = Seq.adjust f (length xs - 1) xs

    toAttrsTuple p = (attrs, p)
      where
        attrs =
          [ term "hx-get" $ linkToText (apiLinks.cd (Just (ClientPath.toClientPath root p)))
          , term "hx-target" ("#" <> viewId)
          , term "hx-swap" "outerHTML"
          ]

    addAttr a = first (class_ a :)

    toLi :: ([Attribute], FilePath) -> Html ()
    toLi (attrs, p) = li_ attrs . toHtml . pathShow $ p

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
  :: Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Html () -> Maybe (Html ())
  -> Bool -> ControlPanelState -> Html ()
controlPanel
  newFolderBtn
  newFileBtn
  uploadBtn
  copyBtn
  pasteBtn
  deleteBtn
  cancelBtn
  mScroll2TopBtn
  readOnly state = do
  case readOnly of
    True ->
      div_ [ id_ controlPanelId ] do
        span_ [ class_ "btn-like field " ] do
          i_ [ class_ "bx bx-lock-alt" ] mempty
          span_ "Read-only"
        maybe mempty id mScroll2TopBtn
    False ->
      case state of
        ControlPanelDefault ->
          div_ [ id_ controlPanelId ] do
            newFolderBtn
            newFileBtn
            uploadBtn
            maybe mempty id mScroll2TopBtn
        ControlPanelSelecting ->
          div_ [ id_ controlPanelId ] do
            newFolderBtn
            newFileBtn
            uploadBtn
            copyBtn
            deleteBtn
            cancelBtn
            maybe mempty id mScroll2TopBtn
        ControlPanelCopied ->
          div_ [ id_ controlPanelId ] do
            newFolderBtn
            newFileBtn
            uploadBtn
            pasteBtn
            cancelBtn
            maybe mempty id mScroll2TopBtn


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
