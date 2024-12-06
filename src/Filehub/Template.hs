{-# LANGUAGE QuasiQuotes #-}

module Filehub.Template where

import Lucid
import Lens.Micro
import Lens.Micro.Platform ()
import System.FilePath (splitPath, takeFileName)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as LText
import Data.ByteString.Lazy qualified as LBS
import Data.String.Interpolate (iii)
import Data.Foldable (traverse_, Foldable (..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..))
import Data.Bifunctor (Bifunctor(..))
import Text.Fuzzy (simpleFilter)
import Filehub.Domain (File (..), FileContent (..), SearchWord (..), SortFileBy (..), sortFiles, ClientPath (..))
import Servant (ToHttpApiData(..))
import Filehub.Domain qualified as Domain
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)


------------------------------------
-- components
------------------------------------


index :: Html ()
      -> Html ()
index view' = do
  withDefault do
    div_ [ id_ "index" ] do
      controlPanel
      view'


controlPanel :: Html ()
controlPanel = do
    div_ [ id_ elementId ] do
      newFolderBtn
      newFileBtn
      uploadBtn
      sortByBtn
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
          [ term "hx-get" ("/cd?dir=" <> toClientPath root p)
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


------------------------------------
-- buttons
------------------------------------


newFolderBtn :: Html ()
newFolderBtn =
  button_ [ class_ "btn btn-control "
          , type_ "submit"
          , term "hx-get" "/modal/new-folder"
          , term "hx-target" "body"
          , term "hx-swap" "beforeend"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bx-folder-plus" ] mempty
      "New Folder"


newFileBtn :: Html ()
newFileBtn  =
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/modal/new-file"
          , term "hx-target" "body"
          , term "hx-swap" "beforeend"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bxs-file-plus" ] mempty
      "New File"


uploadBtn :: Html ()
uploadBtn = do
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/modal/upload"
          , term "hx-target" "body"
          , term "hx-swap" "beforeend"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bx-upload" ] mempty
      "Upload"


sortByBtn :: Html ()
sortByBtn = do
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/dropdown/sortby/on"
          , term "hx-swap" "outerHTML"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bx-sort" ] mempty
      "Sort"


sortByDropdownOn :: Html ()
sortByDropdownOn = do
  button_ [ class_ "btn btn-control"
          , id_ "sortby-btn-with-dropdown"
          , type_ "submit"
          , term "_" "on click trigger closeDropdown on the next .dropdown"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bx-sort" ] mempty
      "Sort"

  dropdown [ id_ componentIds.sortByDropdown
           , term "hx-get" "/dropdown/sortby/off"
           , term "hx-swap" "outerHTML"
           , term "hx-target" "#sortby-btn-with-dropdown"
           ] do
    dropdownItem $
      span_ [ term "hx-get" "/table/sort"
            , term "hx-vals" $ [ "by" .= toUrlPiece ByName ] & toHxVals
            , term "hx-swap" "outerHTML"
            , term "hx-target" "#view" ] "Name"
    dropdownItem $
      span_ [ term "hx-get" "/table/sort"
            , term "hx-vals" $ [ "by" .= toUrlPiece ByModified ] & toHxVals
            , term "hx-swap" "outerHTML"
            , term "hx-target" "#view" ] "Modified"
    dropdownItem $
      span_ [ term "hx-get" "/table/sort"
            , term "hx-vals" $ [ "by" .= toUrlPiece BySize ] & toHxVals
            , term "hx-swap" "outerHTML"
            , term "hx-target" "#view" ] "Size"


sortByDropdownOff :: Html ()
sortByDropdownOff = sortByBtn


infoBtn :: Html ()
infoBtn =
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/modal/info"
          , term "hx-target" "body"
          , term "hx-swap" "beforeend"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bxs-info-circle" ] mempty
      "Info"


contextMenu :: ClientPath -> Html ()
contextMenu (ClientPath clientPath) = do
  dropdown [ class_ "file-contextmenu " ] do
    dropdownItem do
      span_ [ term "hx-get" "/modal/editor"
            , term "hx-vals" $ [ "file" .= Text.pack clientPath ] & toHxVals
            , term "hx-target" "body"
            , term "hx-swap" "beforeend"
            ]
            "Open"

    dropdownItem do
      span_ [ term "hx-get" "/table/sort"
            , term "hx-vals" $ [ "by" .=  toUrlPiece ByModified ] & toHxVals
            , term "hx-swap" "outerHTML"
            , term "hx-target" "#view"
            ]
            "Rename"

    dropdownItem do
      span_ [ term "hx-delete" "/files/delete"
            , term "hx-swap" "outerHTML"
            , term "hx-encoding""application/x-www-form-urlencoded"
            , term "hx-target" "#index"
            , term "hx-vals" $ [ "file" .= Text.pack clientPath ] & toHxVals
            , term "hx-confirm" "Are you sure?"
            ]
            "Delete"

    dropdownItem $ a_ [ href_ ("/download?file=" <> Text.pack clientPath ) ] "Download"

    dropdownItem do
      span_ [ term "hx-get" "/modal/file/detail"
            , term "hx-vals" $ [ "file" .= Text.pack clientPath ] & toHxVals
            , term "hx-target" "body"
            , term "hx-swap" "beforeend"
            ]
            "Details"

------------------------------------
-- search
------------------------------------


search :: SearchWord -> FilePath -> [File] -> Html ()
search (SearchWord searchWord) root files = do
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  table root (sortFiles ByName filteredFiles)


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
           , placeholder_ "New file name"
           , term "hx-post" "/files/new"
           ]
    br_ mempty >> br_ mempty
    button_ [ class_ "btn btn-modal-confirm mr-2 "
            , term "_" "on click trigger closeModal"
            ] "CREATE"

    button_ [ class_ "btn btn-modal-close "
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
           , placeholder_ "New folder name"
           , term "hx-post" "/folders/new"
           ]
    br_ mempty >> br_ mempty
    button_ [ class_ "btn btn-modal-confirm mr-2 "
            , term "_" "on click trigger closeModal"
            ] "CREATE"

    button_ [ class_ "btn btn-modal-close "
            , term "_" "on click trigger closeModal"
            ] "CLOSE"


fileDetailModal :: File -> Html ()
fileDetailModal file = do
  modal [ id_ componentIds.fileDetailModal ] do
    "Detail"
    br_ mempty >> br_ mempty

    table_ do
      tbody_ do
        tr_ do
          td_ "Filename"
          td_ (toHtml $ takeFileName file.path)
        tr_ do
          td_ "Path"
          td_ (toHtml file.path)


    button_ [ class_ "btn btn-modal-close "
            , term "_" "on click trigger closeModal"
            ] "CLOSE"


uploadModal :: Html ()
uploadModal = do
  modal [ id_ componentIds.updateModal ] do
    "Upload"
    br_ mempty >> br_ mempty
    form_ [ term "hx-encoding" "multipart/form-data"
          , term "hx-post" "/upload"
          ] do
      input_ [ class_ "btn btn-control "
             , type_ "file"
             , name_ "file"
             ]

      br_ mempty >> br_ mempty

      button_ [ class_ "btn btn-modal-confirm mr-2 "
              , term "_" "on click trigger closeModal"
              ] "UPLOAD"

      button_ [ class_ "btn btn-modal-close "
              , term "_" "on click trigger closeModal"
              ] "CLOSE"


editorModal :: FilePath -> LBS.ByteString -> Html ()
editorModal filename content = do

  modal [ id_ componentIds.editorModal ] do
    "Edit"

    br_ mempty >> br_ mempty

    form_ [ term "hx-put" "/files/update"
          , term "hx-confirm" "Save the edit?"
          ] do
      input_ [ class_ "form-control "
             , type_ "text"
             , name_ "path"
             , value_ (Text.pack filename)
             , placeholder_ "Filename"
             ]

      br_ mempty >> br_ mempty

      textarea_
        [ class_ "form-control "
        , type_ "text"
        , name_ "content"
        , placeholder_ "Empty File"
        ]
        (toHtml $ LText.decodeUtf8 content)

      br_ mempty >> br_ mempty

      button_ [ class_ "btn btn-modal-confirm mr-2 "
              , term "_" "on click trigger closeModal"
              ] "UPLOAD"

      button_ [ class_ "btn btn-modal-close "
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
    div_ [ class_ "modal-underlay "
         , term "_" "on click trigger closeModal"
         ] mempty
    div_ [ class_ "modal-content " ] do
      body
  where
    closeModalScript = term "_"
      [iii|
        on closeModal
        add .closing
        then wait for animationend
        then remove me
      |]


-- | Dropdown's underlay and content are sibiling so we can have the underlay
--   cover the entire page while has the content being positioned relative to the parent.
dropdown :: [Attribute] -> Html () -> Html ()
dropdown attrs body = do
  div_ [ class_ "dropdown-underlay "
       , term "_" "on click trigger closeDropdown on the next .dropdown"
       ] mempty
  div_ ([ class_ "dropdown ", closeDropdownScript
        , term "hx-trigger" "epilogue"
        ] <> attrs) do
    div_ [ class_ "dropdown-content " ] body
  where
    closeDropdownScript = term "_"
      [iii|
        on closeDropdown
        trigger epilogue
        then add .closing
        then wait for animationend
        then remove the previous .dropdown-underlay
        then remove me
      |]


dropdownItem :: Html () -> Html ()
dropdownItem body = div_ [ class_ "dropdown-item " ] body


------------------------------------
-- table
------------------------------------


table :: FilePath -> [File] -> Html ()
table root files = do
  table_ [ id_ componentIds.table ] do
    thead_ do
      tr_ do
        th_ [ id_ "table-name" ] "Name"
        th_ [ id_ "table-modified" ] "Modified"
        th_ [ id_ "table-size" ] "Size"
    tbody_ $ do
      traverse_
        (\file -> do
          tr_  do
            td_ $ fileNameElement file
            td_ $ modifiedDateElement file
            td_ $ sizeElement file)
        files
  where
    contextMenuTrigger file =
      [ term "_"
          [iii|
            on contextmenu
            halt the event
            then trigger filhubContextmenu
          |]
       , term "hx-get" ("/contextmenu/file?path=" <> toClientPath root file.path)
       , term "hx-swap" "afterend"
       , term "hx-trigger" "filhubContextmenu"
       ]

    sizeElement :: File -> Html ()
    sizeElement file =
      span_ (toHtml . show $ file.size)
        `with` [ class_ "field "]
        `with` contextMenuTrigger file

    modifiedDateElement :: File -> Html ()
    modifiedDateElement file =
      span_ (toHtml $ formatTime defaultTimeLocale "%F %R" file.mtime)
        `with` [ class_ "field "]
        `with` contextMenuTrigger file

    fileNameElement :: File -> Html ()
    fileNameElement file = do
      span_ (icon >> name)
        `with` [ class_ "field " ]
        `with` contextMenuTrigger file
      where
        name = span_ (mconcat [ cdAttrs, otherAttrs ]) (toHtml . takeFileName $ file.path)

        icon =
          case file.content of
            Dir _ -> i_ [ class_ "bx bxs-folder "] mempty
            Content -> i_ [ class_ "bx bxs-file-blank "] mempty
        cdAttrs =
          mconcat
            [ case file.content of
                Dir _ ->
                  [ term "hx-get" ("/cd?dir=" <> toClientPath root file.path)
                  , term "hx-target" ("#" <> componentIds.view)
                  , term "hx-swap" "outerHTML"
                  ]
                Content -> mempty
            ]
        otherAttrs =
          case file.content of
            Dir _ -> [ class_ "dir " ]
            _ -> []


------------------------------------
-- component ids
------------------------------------


data ComponentIds = ComponentIds
  { view :: Text
  , controlPanel :: Text
  , searchBar :: Text
  , pathBreadcrumb :: Text
  , table :: Text
  , newFileModal :: Text
  , newFolderModal :: Text
  , fileDetailModal :: Text
  , updateModal :: Text
  , sortByDropdown :: Text
  , editorModal :: Text
  }
  deriving Show


componentIds :: ComponentIds
componentIds = ComponentIds
  { view = "view"
  , controlPanel = "control-panel"
  , searchBar = "search-bar"
  , pathBreadcrumb = "path-breadcrumb"
  , table = "table"
  , newFileModal = "new-file-modal"
  , newFolderModal = "new-folder-modal"
  , fileDetailModal = "file-detail-modal"
  , updateModal = "update-modal"
  , sortByDropdown = "sortby-dropdown"
  , editorModal = "editor-modal"
  }


------------------------------------
-- helpers
------------------------------------


toClientPath :: FilePath -> FilePath -> Text
toClientPath root p = Text.pack . (.unClientPath) $ Domain.toClientPath root p


toHxVals :: [Pair] -> Text
toHxVals xs = (xs & Aeson.object & Aeson.encode & LText.decodeUtf8) ^. strict
