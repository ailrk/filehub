{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module Filehub.Template where

import Lucid
import Lens.Micro
import Lens.Micro.Platform ()
import System.FilePath (splitPath, takeFileName)
import Network.URI.Encode qualified as URI.Encode
import Servant (ToHttpApiData(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy.Encoding qualified as LText
import Text.Fuzzy (simpleFilter)
import Data.ByteString.Lazy qualified as LBS
import Data.String.Interpolate (iii)
import Data.Foldable (traverse_, Foldable (..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)
import Filehub.Domain (File (..), FileContent (..), SearchWord (..), SortFileBy (..), sortFiles, ClientPath (..))
import Filehub.Domain qualified as Domain
import Network.Mime (MimeType)


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
          , term "hx-target" "#index"
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
          , term "hx-target" "#index"
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
          , term "hx-target" "#index"
          , term "hx-swap" "beforeend"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bx-upload" ] mempty
      "Upload"


sortByBtn :: Html ()
sortByBtn = do
  div_ [ class_ "dropdown "
       , id_ componentIds.sortByDropdown
       ] do
    button_ [ class_ "btn btn-control dropdown-btn "
            , term "_"
                [iii|
                  on click
                    if (the next .dropdown-content) matches .closed
                    then send show to the next .dropdown-content
                    else send close to the next .dropdown-content
                    end
                  end
                |]
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-sort" ] mempty
        "Sort"

    div_ [ class_ "dropdown-content closed "
         , term "_"
            [iii|
              init hide me end

              on close
                remove .show
                then add .closing
                then wait for animationend
                then remove .closing
                then hide me
                then add .closed
              end

              on show
                remove .closed
                then show me
              end
            |]
         ] do

      div_ [ class_ "dropdown-item"
           , term "hx-get" "/table/sort"
           , term "hx-vals" $ [ "by" .= toUrlPiece ByName ] & toHxVals
           , term "hx-swap" "outerHTML"
           , term "hx-target" "#view" ] $
        span_ "Name"

      div_ [ class_ "dropdown-item"
           , term "hx-get" "/table/sort"
           , term "hx-vals" $ [ "by" .= toUrlPiece ByModified ] & toHxVals
           , term "hx-swap" "outerHTML"
           , term "hx-target" "#view" ] $
        span_ "Modified"

      div_ [ class_ "dropdown-item"
           , term "hx-get" "/table/sort"
           , term "hx-vals" $ [ "by" .= toUrlPiece BySize ] & toHxVals
           , term "hx-swap" "outerHTML"
           , term "hx-target" "#view"
           ] $
        span_ "Size"


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
           ]


------------------------------------
-- modals
------------------------------------


newFileModal :: Html ()
newFileModal = do
  modal [ id_ componentIds.newFileModal ] do
    "File"
    br_ mempty >> br_ mempty
    form_ [ term "hx-post" "/files/new"
          , term "hx-target" "#view"
          , term "hx-swap" "outerHTML"
          ] do
      input_ [ class_ "form-control "
             , type_ "text"
             , name_ "new-file"
             , placeholder_ "New file name"
             ]
      br_ mempty >> br_ mempty
      button_ [ class_ "btn btn-modal-confirm mr-2 "
              , type_ "submit"
              , term "_" "on click trigger close"
              ] "CREATE"

      button_ [ class_ "btn btn-modal-close "
              , type_ "button" -- prevent submission
              , term "_" "on click trigger close"
              ] "CLOSE"


newFolderModal :: Html ()
newFolderModal = do
  modal [ id_ componentIds.newFolderModal ] do
    "File"
    br_ mempty >> br_ mempty
    form_ [ term "hx-post" "/folders/new"
          , term "hx-target" "#view"
          , term "hx-swap" "outerHTML"
          ] do
      input_ [ class_ "form-control "
             , type_ "text"
             , name_ "new-folder"
             , placeholder_ "New folder name"
             ]
      br_ mempty >> br_ mempty
      button_ [ class_ "btn btn-modal-confirm mr-2 "
              , type_ "submit"
              , term "_" "on click trigger close"
              ] "CREATE"
      button_ [ class_ "btn btn-modal-close "
              , type_ "button"
              , term "_" "on click trigger close"
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
          td_ "Modified"
          td_ (toHtml $ formatTime defaultTimeLocale "%F %R" file.mtime)
        tr_ do
          td_ "Accessed"
          td_ (toHtml $ formatTime defaultTimeLocale "%F %R" file.atime)
        tr_ do
          td_ "Size"
          td_ (toHtml $ show file.size)


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
              , term "_" "on click trigger close"
              ] "UPLOAD"

      button_ [ class_ "btn btn-modal-close "
              , term "_" "on click trigger close"
              ] "CLOSE"


editorModal :: FilePath -> LBS.ByteString -> Html ()
editorModal filename content = do

  modal [ id_ componentIds.editorModal ] do
    "Edit"

    br_ mempty >> br_ mempty

    form_ [ term "hx-put" "/files/update"
          , term "hx-confirm" ("Save the edit of " <> Text.pack filename <> "?")
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
              , term "_" "on click trigger close"
              ] "EDIT"

      button_ [ class_ "btn btn-modal-close "
              , type_ "button"
              , term "_" "on click trigger close"
              ] "CLOSE"


imageModal :: ClientPath -> Html ()
imageModal (ClientPath path) = do
  img_ [ src_  (Text.pack path) ]


------------------------------------
-- default
------------------------------------


withDefault :: Html () -> Html ()
withDefault html = do
  meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0" ]
  link_ [ rel_ "stylesheet", href_ "/static/style.css" ]
  link_ [ rel_ "stylesheet", href_ "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css" ]

  link_ [ rel_ "stylesheet", href_ "/static/viewerjs/viewer.css" ]
  script_ [ src_ "/static/viewerjs/viewer.js" ] ("" :: Text)
  script_ [ src_ "https://unpkg.com/hyperscript.org@0.9.13" ] ("" :: Text)
  script_ [ src_ "https://unpkg.com/htmx.org@2.0.3" ] ("" :: Text)
  script_ [ src_ "/static/ui.js" ] ("" :: Text)
  html


------------------------------------
-- elements
------------------------------------


modal :: [Attribute] -> Html () -> Html ()
modal attrs body = do
  div_ ([ class_ "modal ", closeModalScript ] <> attrs) do
    underlay
    div_ [ class_ "modal-content " ] do
      body
  where
    closeModalScript = term "_"
      [iii|
        on close
          add .closing
          then wait for animationend
          then remove me
        end
      |]

    underlay = do
        div_ [ class_ "modal-underlay "
             , term "_"
                [iii|
                  on click
                  send close to .modal
                  end
                |]
             ] mempty


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
    tbody_ $ traverse_ record files
  where
    record :: File -> Html ()
    record file =
      tr_ attrs do
        td_ $ fileNameElement file
        td_ $ modifiedDateElement file
        td_ $ sizeElement file
      where
        attrs :: [Attribute]
        attrs =
          [ term "_"
              [iii|
                on contextmenu(pageX, pageY)
                halt the event
                then if \##{contextMenuId} exists then remove \##{contextMenuId} end
                then fetch /contextmenu?file=#{path}
                then put result after #{tableId}
                then send show( pageX: pageX
                              , pageY: pageY
                              , path: "#{path}"
                              )
                     to \##{contextMenuId}
              |]
           ]
        path = let ClientPath p = Domain.toClientPath root file.path
                in URI.Encode.encode p
        tableId = componentIds.table
        contextMenuId = componentIds.contextMenu


    sizeElement :: File -> Html ()
    sizeElement file =
      span_ (toHtml . Domain.toReadableSize $ file.size)
        `with` [ class_ "field "]


    modifiedDateElement :: File -> Html ()
    modifiedDateElement file =
      span_ (toHtml $ formatTime defaultTimeLocale "%F %R" file.mtime)
        `with` [ class_ "field "]


    fileNameElement :: File -> Html ()
    fileNameElement file = do
      span_ (icon >> name)
        `with` [ class_ "field " ]
      where
        name =
          span_ (toHtml . takeFileName $ file.path) `with`
            mconcat
              [ case file.content of
                  Dir _ ->
                    [ term "hx-get" ("/cd?dir=" <> toClientPath root file.path)
                    , term "hx-target" ("#" <> componentIds.view)
                    , term "hx-swap" "outerHTML"
                    ]
                  Content
                    | file.mimetype `isMime` "image" ->
                        [ term "hx-get" "/modal/image"
                        , term "hx-vals" $ [ "file" .= toClientPath root file.path ] & toHxVals
                        , term "hx-target" "#index"
                        , term "hx-swap" "beforeend"
                        ]
                    | otherwise ->
                        [ term "hx-get" "/modal/editor"
                        , term "hx-vals" $ [ "file" .= toClientPath root file.path ] & toHxVals
                        , term "hx-target" "#index"
                        , term "hx-swap" "beforeend"
                        ]
              , case file.content of
                  Dir _ -> [ class_ "dir " ]
                  _ -> mempty
              ]

        icon =
          case file.content of
            Dir _ -> i_ [ class_ "bx bxs-folder "] mempty
            Content -> i_ [ class_ "bx bxs-file-blank "] mempty


contextMenu :: ClientPath -> File -> Html ()
contextMenu (ClientPath clientPath) file = do
  div_ [ class_ "dropdown-content "
       , id_ componentIds.contextMenu
       , term "_"
           [iii|
             init call htmx.process(me) end
             on close
               add .closing
               then wait for animationend
               then remove me
             end

             on show(pageX, pageY, path)
               set my *left to pageX
               then set my *top to pageY
               then set my *position to 'absolute'
               then show me
             end
         |]
      ] do

    case file.content of
      Dir _ -> do
        div_ [ class_ "dropdown-item"
             , term "hx-get" ("/cd?dir=" <> Text.pack clientPath )
             , term "hx-target" ("#" <> componentIds.view)
             , term "hx-swap" "outerHTML"
             ] $
          span_ "Open"
      Content
        | file.mimetype `isMime` "text" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" "/modal/editor"
               , term "hx-vals" $ [ "file" .= Text.pack clientPath ] & toHxVals
               , term "hx-target" "#index"
               , term "hx-swap" "beforeend"
               ] $
            span_ "Edit"
        | file.mimetype `isMime` "image" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" "/modal/image"
               , term "hx-vals" $ [ "file" .= Text.pack clientPath ] & toHxVals
               , term "hx-target" "#index"
               , term "hx-swap" "beforeend"
               ] $
            span_ "View"
        | otherwise ->
            mempty

    div_ [ class_ "dropdown-item" ] $
      a_ [ href_ ("/download?file=" <> Text.pack clientPath ) ] "Download"

    div_ [ class_ "dropdown-item"
         , term "hx-delete" "/files/delete"
         , term "hx-vals" $ [ "file" .= Text.pack clientPath ] & toHxVals
         , term "hx-target" "#view"
         , term "hx-swap" "outerHTML"
         , term "hx-confirm" ("Are you sure about deleting " <> Text.pack clientPath <> "?")
         ] $
      span_ "Delete"

    div_ [ class_ "dropdown-item"
         , term "hx-get" "/modal/file/detail"
         , term "hx-vals" $ [ "file" .= Text.pack clientPath ] & toHxVals
         , term "hx-target" "#index"
         , term "hx-swap" "beforeend"
         ] $
      span_ "Details"

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
  , imageModal :: Text
  , contextMenu :: Text
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
  , imageModal = "image-modal"
  , contextMenu = "contextmenu"
  }


------------------------------------
-- helpers
------------------------------------


toClientPath :: FilePath -> FilePath -> Text
toClientPath root p = Text.pack . (.unClientPath) $ Domain.toClientPath root p


toHxVals :: [Pair] -> Text
toHxVals xs = (xs & Aeson.object & Aeson.encode & LText.decodeUtf8) ^. strict


isMime :: MimeType -> Text -> Bool
isMime fileMime mime = mime `Text.isPrefixOf` Text.decodeUtf8 fileMime
