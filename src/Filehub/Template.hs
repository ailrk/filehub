{-# LANGUAGE QuasiQuotes #-}

module Filehub.Template where

import Lucid
import Lens.Micro
import System.FilePath (splitPath, takeFileName)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.String.Interpolate (iii)
import Data.Foldable (traverse_, Foldable (..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..))
import Data.Bifunctor (Bifunctor(..))
import Text.Fuzzy (simpleFilter)
import Filehub.Domain (File (..), FileContent (..), SearchWord (..), SortFileBy (..), sortFiles)
import Servant (ToHttpApiData(..))


------------------------------------
-- components
------------------------------------


index :: Html ()
      -> Html ()
      -> Html ()
index view' tree' = do
  withDefault do
    div_ [ class_ "filehub " ] do
      controlPanel
      tree'
      view'


controlPanel :: Html ()
controlPanel = do
    div_ [ id_ elementId ] do
      newFolderBtn
      newFileBtn
      uploadBtn
      sortByBtn
      viewTypeBtn
      infoBtn
  where
    elementId = componentIds.controlPanel


view :: Html () -> Html () -> Html ()
view table' pathBreadcrumb' = do
  div_ [ id_ componentIds.view ] do
    div_ [ id_ "tool-bar" ] do
      pathBreadcrumb'
      searchBar
    table'


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
  div_ [ class_ "breadcrumb", id_ componentIds.pathBreadcrumb ] do
    ol_ breadcrumbItems
  where
    adjustLast f xs = Seq.adjust f (length xs - 1) xs

    toAttrsTuple p = (attrs, p)
      where
        attrs =
          [ term "hx-get" ("/cd?dir=" <> Text.pack p)
          , term "hx-target" ("#" <> componentIds.view)
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



tree :: File -> Html ()
tree root = do
  ul_ [ id_ elementId ] $ renderFile 0 root
  where
    elementId = componentIds.tree

    cdAttrs p =
      [ term "hx-get" ("/cd?dir=" <> Text.pack p)
      , term "hx-target" ("#" <> componentIds.view)
      , term "hx-swap" "outerHTML"
      ]

    treeFoldAttrs p =
      [ term "hx-get" ("/dirs?path=" <> Text.pack p)
      , term "hx-swap" "outerHTML"
      , term "hx-target" ("#" <> elementId)
      ]

    indent n = term "indent" (Text.pack . show $ n)

    renderFile :: Int -> File -> Html ()
    renderFile n file = do
      let path = file.path
      let name = toHtml . takeFileName $ path
      case file.content of
        Content _ -> do
          li_ [ class_ "dirtree-entry file-name", indent n ] name

        Dir Nothing -> do
          let icon = i_ [ class_ "bx bx-caret-right"] mempty
          li_ [ class_ "dirtree-entry dir file-name", indent n ] do
            span_ (treeFoldAttrs path) icon
            span_ (cdAttrs path) name

        Dir (Just files) -> do
          let icon = i_ [ class_ "bx bx-caret-down"] mempty
          li_ [ class_ "dirtree-entry dir file-name", indent n ] do
            span_ (treeFoldAttrs path) icon
            span_ (cdAttrs path) name
          traverse_ (renderFile (n + 1)) files


------------------------------------
-- buttons
------------------------------------


newFolderBtn :: Html ()
newFolderBtn =
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/modal/new-folder"
          , term "hx-target" "body"
          , term "hx-swap" "beforeend"
          ] "New Folder"


newFileBtn :: Html ()
newFileBtn  =
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/modal/new-file"
          , term "hx-target" "body"
          , term "hx-swap" "beforeend"
          ] "New File"


uploadBtn :: Html ()
uploadBtn = do
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/modal/upload"
          , term "hx-target" "body"
          , term "hx-swap" "beforeend"
          ] "Upload"


sortByBtn :: Html ()
sortByBtn = do
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/dropdown/sortby/on"
          , term "hx-swap" "outerHTML"
          ] "Sort By"


sortByDropdownOn :: Html ()
sortByDropdownOn = do
  button_ [ class_ "btn btn-control"
          , id_ "sortby-btn-with-dropdown"
          , type_ "submit"
          , term "_" "on click trigger closeDropdown on the next .dropdown"
          ] "Sort By"
  dropdown [ id_ componentIds.sortByDropdown
           , term "hx-get" "/dropdown/sortby/off"
           , term "hx-swap" "outerHTML"
           , term "hx-target" "#sortby-btn-with-dropdown"
           ] do

    dropdownItem $ span_ [ term "hx-get" ("/table/sort?by=" <> toUrlPiece ByName), term "hx-swap" "outerHTML", term "hx-target" "#view" ] "Name"
    dropdownItem $ span_ [ term "hx-get" ("/table/sort?by=" <> toUrlPiece ByModified), term "hx-swap" "outerHTML", term "hx-target" "#view" ] "Modified"
    dropdownItem $ span_ [ term "hx-get" ("/table/sort?by=" <> toUrlPiece BySize), term "hx-swap" "outerHTML", term "hx-target" "#view" ]"Size"


dropdownItem :: Html () -> Html ()
dropdownItem body = div_ [ class_ "dropdown-item" ] body


sortByDropdownOff :: Html ()
sortByDropdownOff = sortByBtn


viewTypeBtn :: Html ()
viewTypeBtn = button_ [class_ "btn btn-control", type_ "submit"] "view"


infoBtn :: Html ()
infoBtn =
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/modal/info"
          , term "hx-target" "body"
          , term "hx-swap" "beforeend"
          ] "info"


------------------------------------
-- search
------------------------------------


search :: SearchWord -> [File] -> Html ()
search (SearchWord searchWord) files = do
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  table (sortFiles ByName filteredFiles)



searchBar :: Html ()
searchBar = do
  div_ [ id_ componentIds.searchBar ] do
    input_ [ class_ "form-control "
           , type_ "search"
           , name_ "search"
           , placeholder_ "Search as you type"
           , term "hx-post" "/search"
           , term "hx-trigger" "input changed delay:200ms, search"
           , term "hx-target" "#table"
           , term "hx-swap" "outerHTML"
           , term "hx-indicator" ".htmx-indicator"
           ]


------------------------------------
-- modals
------------------------------------


infoModal :: Html ()
infoModal = do
  modal [ id_ componentIds.newFileModal ] do
    "Storage"


newFileModal :: Html ()
newFileModal = do
  modal [ id_ componentIds.newFileModal ] do
    "File"
    br_ mempty >> br_ mempty
    input_ [ class_ "form-control "
           , type_ "text"
           , name_ "new-file"
           , placeholder_ "Search as you type"
           , term "hx-put" "/files/new"
           ]
    br_ mempty >> br_ mempty
    button_ [ class_ "btn btn-modal-confirm mr-2"
            , term "_" "on click trigger closeModal"
            ] "CREATE"

    button_ [ class_ "btn btn-modal-close"
            , term "_" "on click trigger closeModal"
            ] "CLOSE"


newFolderModal :: Html ()
newFolderModal = do
  modal [ id_ componentIds.newFolderModal ] do
    "Folder"
    br_ mempty >> br_ mempty
    input_ [ class_ "form-control "
           , type_ "text"
           , name_ "new-folder"
           , placeholder_ "Search as you type"
           , term "hx-put" "/folders/new"
           ]
    br_ mempty >> br_ mempty
    button_ [ class_ "btn btn-modal-confirm mr-2"
            , term "_" "on click trigger closeModal"
            ] "CREATE"

    button_ [ class_ "btn btn-modal-close"
            , term "_" "on click trigger closeModal"
            ] "CLOSE"


uploadModal :: Html ()
uploadModal = do
  modal [ id_ componentIds.updateModal ] do
    "Upload"
    br_ mempty >> br_ mempty
    form_ [ term "hx-encoding" "multipart/form-data"
          , term "hx-post" "/upload"
          , term "_" "on htmx:xhr:progress(loaded, total) set #progress.value to (loaded/total)*100"
          ] do
      input_ [ class_ "btn btn-control"
             , type_ "file"
             , name_ "file"
             ]

      br_ mempty >> br_ mempty

      button_ [ class_ "btn btn-modal-confirm mr-2"
              , term "_" "on click trigger closeModal"
              ] "UPLOAD"

      button_ [ class_ "btn btn-modal-close"
              , term "_" "on click trigger closeModal"
              ] "CLOSE"


------------------------------------
-- default
------------------------------------


withDefault :: Html () -> Html ()
withDefault html = do
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
  link_ [rel_ "stylesheet", href_ "/static/style.css" ]
  link_ [rel_ "stylesheet", href_ "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css" ]

  script_ [src_ "https://unpkg.com/hyperscript.org@0.9.13"] ("" :: Text)
  script_ [src_ "https://unpkg.com/htmx.org@2.0.3"] ("" :: Text)
  html


------------------------------------
-- elements
------------------------------------


modal :: [Attribute] -> Html () -> Html ()
modal attrs body = do
  div_ ([ class_ "modal ", closeModalScript ] <> attrs) do
    div_ [ class_ "modal-underlay"
         , term "_" "on click trigger closeModal"
         ] mempty
    div_ [ class_ "modal-content" ] do
      body
  where
    closeModalScript = term "_"
      [iii|
        on closeModal
        add .closing
        then wait for animationend
        then remove me
      |]


dropdown :: [Attribute] -> Html () -> Html ()
dropdown attrs body = do
  div_ [ class_ "dropdown-wrapper" ] do
    div_ [ class_ "dropdown-underlay"
         , term "_" "on click trigger closeDropdown on the next .dropdown"
         ] mempty
    div_ ([ class_ "dropdown", closeDropdownScript
          , term "hx-trigger" "epilogue"
          ] <> attrs) do
      div_ [ class_ "dropdown-content "
           ] body
  where
    closeDropdownScript = term "_"
      [iii|
        on closeDropdown
        trigger epilogue
        then add .closing
        then wait for animationend
        then remove the closest .dropdown-wrapper
      |]


------------------------------------
-- table
------------------------------------


table :: [File] -> Html ()
table files = do
  table_ [ id_ componentIds.table ] do
    thead_ do
      tr_ do
        th_ [ id_ "table-name" ] "Name"
        th_ [ id_ "table-modified" ] "Modified"
        th_ [ id_ "table-size" ] "Size"
    tbody_ $ do
      traverse_
        (\file@(File { size = size, mtime = mtime }) -> do
          tr_ do
            td_
              (fileNameElement file)
            td_ (toHtml $ formatTime defaultTimeLocale "%F %R" mtime)
            td_ (toHtml . show $ size))
        files
  where
    fileNameElement :: File -> Html ()
    fileNameElement file = do
      span_ [ class_ "file-name" ] do
        icon
        span_ attrs do
          toHtml . takeFileName $ file.path
      where
        icon =
          case file.content of
            Dir _ -> i_ [ class_ "bx bxs-folder"] mempty
            Content _ -> i_ [ class_ "bx bxs-file-blank"] mempty
        cdAttrs =
          [ class_ "breadcrumb-item "
          , term "hx-get" ("/cd?dir=" <> Text.pack file.path)
          , term "hx-target" ("#" <> componentIds.view)
          , term "hx-swap" "outerHTML"
          ]
        otherAttrs =
          case file.content of
            Dir _ -> [ class_ "dir " ]
            _ -> []
        attrs = mconcat [ cdAttrs, otherAttrs ]



------------------------------------
-- component ids
------------------------------------


data ComponentIds = ComponentIds
  { tree :: Text
  , view :: Text
  , controlPanel :: Text
  , searchBar :: Text
  , pathBreadcrumb :: Text
  , table :: Text
  , newFileModal :: Text
  , newFolderModal :: Text
  , updateModal :: Text
  , sortByDropdown :: Text
  }
  deriving Show


componentIds :: ComponentIds
componentIds = ComponentIds
  { tree = "tree"
  , view = "view"
  , controlPanel = "control-panel"
  , searchBar = "search-bar"
  , pathBreadcrumb = "path-breadcrumb"
  , table = "table"
  , newFileModal = "new-file-modal"
  , newFolderModal = "new-folder-modal"
  , updateModal = "update-modal"
  , sortByDropdown = "sortby-dropdown"
  }
