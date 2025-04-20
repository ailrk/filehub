{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE NamedFieldPuns #-}

module Filehub.Template
  ( withDefault
  , index
  , sideBar
  , controlPanel
  , view
  , pathBreadcrumb
  , newFileModal
  , newFolderModal
  , fileDetailModal
  , uploadModal
  , editorModal
  , search
  , contextMenu
  , table
  )
  where

import Lucid
import Lens.Micro
import Lens.Micro.Platform ()
import System.FilePath (splitPath, takeFileName)
import Network.URI.Encode qualified as URI.Encode
import Servant (ToHttpApiData(..))
import Data.Text (Text)
import Data.Text qualified as Text
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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Maybe (fromMaybe)
import Filehub.Domain (sortFiles, isMime, ClientPath (..))
import Filehub.Domain.Types (File (..), FileContent (..), SearchWord (..), SortFileBy (..))
import Filehub.Domain qualified as Domain
import Filehub.Domain.Viewer qualified as Viewer
import Filehub.Types (Target (..), S3Target(..), FileTarget(..))
import Filehub.Env.Target qualified as Target
import Filehub.Env.Target (TargetView(..))


------------------------------------
-- components
------------------------------------


index :: Html ()
      -> Html ()
      -> Html ()
index sideBar' view' = do
  div_ [ id_ "index" ] do
    sideBar'
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


sideBar :: [Target] -> TargetView -> Html ()
sideBar targets (TargetView currentTarget _ _) = do
  div_ [ id_ elementId ] do
    traverse_ targetIcon targets
  where
    elementId = componentIds.sideBar

    targetIcon :: Target -> Html ()
    targetIcon target = do
      div_ [ class_ "target-icon"
           , term "hx-get" "/target/change"
           , term "hx-vals" $ [ "target" .= toUrlPiece (Target.getTargetId target)] & toHxVals
           , term "hx-target" "#index"
           , term "hx-swap" "outerHTML"
           ] do
        case target of
          S3Target _ -> do
            i_ [ class_ "bx bxs-cube" ] mempty
          FileTarget _ -> do
            i_ [ class_ "bx bx-folder" ] mempty
      `with` targetAttr target
      `with` targetInfo
      where
        targetAttr t = [class_ " current-target" | Target.getTargetId currentTarget == Target.getTargetId t]
        targetInfo =
          case target of
            S3Target (S3Target_ { bucket }) ->
              [ term "data-target-info" [iii| [S3] #{bucket} |] ]
            FileTarget (FileTarget_ { root }) ->
              [ term "data-target-info" [iii| [FileSystem] #{takeFileName root} |] ]


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
                      then send Show to the next .dropdown-content
                      else send Close to the next .dropdown-content
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
              on Close
                log "Close"
                remove .show
                then hide me
                then add .closed
              end

              on Show
                log "Show"
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


search :: SearchWord -> Target -> FilePath -> [File] -> Html ()
search (SearchWord searchWord) target root files = do
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  table target root (sortFiles ByName filteredFiles)


searchBar :: Html ()
searchBar = do
  div_ [ id_ componentIds.searchBar ] do
    input_ [ class_ "form-control "
           , type_ "input"
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
              , term "_" "on click trigger Close"
              ] "CREATE"

      button_ [ class_ "btn btn-modal-close "
              , type_ "button" -- prevent submission
              , term "_" "on click trigger Close"
              ] "Close"


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
              , term "_" "on click trigger Close"
              ] "CREATE"
      button_ [ class_ "btn btn-modal-close "
              , type_ "button"
              , term "_" "on click trigger Close"
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
          td_ (toHtml $ maybe mempty (formatTime defaultTimeLocale "%F %R") file.mtime)
        tr_ do
          td_ "Accessed"
          td_ (toHtml $ maybe mempty (formatTime defaultTimeLocale "%F %R") file.atime)
        tr_ do
          td_ "Size"
          td_ (toHtml . Domain.toReadableSize $ fromMaybe 0 file.size)
        tr_ do
          td_ "Content Type"
          td_ (toHtml file.mimetype)



uploadModal :: Html ()
uploadModal = do
  modal [ id_ componentIds.uploadModal ] do
    "Upload"
    br_ mempty >> br_ mempty
    form_ [ term "hx-encoding" "multipart/form-data"
          , term "hx-post" "/upload"
          , term "hx-target" "#index"
          , term "hx-swap" "outerHTML"
          ] do
      input_ [ class_ "btn btn-control "
             , type_ "file"
             , name_ "file"
             ]

      br_ mempty >> br_ mempty

      button_ [ class_ "btn btn-modal-confirm mr-2 "
              , term "_" "on click trigger Close"
              ] "UPLOAD"

      button_ [ class_ "btn btn-modal-close "
              , term "_" "on click trigger Close"
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
              , term "_" "on click trigger Close"
              ] "EDIT"

      button_ [ class_ "btn btn-modal-close "
              , type_ "button"
              , term "_" "on click trigger Close"
              ] "CLOSE"


------------------------------------
-- default
------------------------------------


withDefault :: Html () -> Html ()
withDefault html = do
  meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0" ]
  link_ [ rel_ "stylesheet", href_ "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css" ]

  script_ [ src_ "https://unpkg.com/hyperscript.org@0.9.13" ] ("" :: Text)
  script_ [ src_ "https://unpkg.com/htmx.org@2.0.3" ] ("" :: Text)

  link_ [ rel_ "stylesheet", href_ "/static/filehub/viewer.css" ]
  script_ [ src_ "/static/filehub/viewer.js", type_ "module" ] ("" :: Text)

  link_ [ rel_ "stylesheet", href_ "/static/filehub/ui.css" ]
  script_ [ src_ "/static/filehub/ui.js", type_ "module" ] ("" :: Text)
  html


------------------------------------
-- elements
------------------------------------


modal :: [Attribute] -> Html () -> Html ()
modal attrs body = do
  div_ [ class_ "modal ", closeModalScript ] do
    underlay
    div_ ([ class_ "modal-content " ] <> attrs) do
      body
  where
    closeModalScript = term "_"
      [iii|
        on Close
          add .closing
          then wait for animationend
          then remove .closing
          then remove me
        end
      |]

    underlay = do
        div_ [ class_ "modal-underlay "
             , term "_"
                [iii|
                  on click
                  send Close to .modal
                  end
                |]
             ] mempty


------------------------------------
-- table
------------------------------------


table :: Target -> FilePath -> [File] -> Html ()
table target root files = do
  table_ [ id_ componentIds.table ] do
    thead_ do
      tr_ do
        th_ "Name"
        th_ "Modified"
        th_ "Size"
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
                then send Show( pageX: pageX
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
      span_ (toHtml . Domain.toReadableSize $ fromMaybe 0 file.size)
        `with` [ class_ "field "]


    modifiedDateElement :: File -> Html ()
    modifiedDateElement file =
      span_ (toHtml $ maybe mempty (formatTime defaultTimeLocale "%F %R") file.mtime)
        `with` [ class_ "field "]


    fileNameElement :: File -> Html ()
    fileNameElement file = do
      span_ (icon >> name)
        `with` [ class_ "field " ]
      where
        name =
          span_ (toHtml displayName) `with`
            mconcat
              [ case file.content of
                  Dir _ ->
                    [ term "hx-get" ("/cd?dir=" <> toClientPath root file.path)
                    , term "hx-target" ("#" <> componentIds.view)
                    , term "hx-swap" "outerHTML"
                    ]
                  Content
                    | file.mimetype `isMime` "application/pdf" -> openBlank file
                    | file.mimetype `isMime` "video" || file.mimetype `isMime` "mp4" -> open file
                    | file.mimetype `isMime` "audio" || file.mimetype `isMime` "mp3" -> open file
                    | file.mimetype `isMime` "image" -> open file
                    | otherwise -> editor file
              , case file.content of
                  Dir _ -> [ class_ "dir " ]
                  _ -> mempty
              ]
          where
            displayName =
              case target of
                S3Target _ -> file.path
                FileTarget _ -> takeFileName file.path

        icon =
          case file.content of
            Dir _ -> i_ [ class_ "bx bxs-folder "] mempty
            Content -> i_ [ class_ "bx bxs-file-blank "] mempty

    openBlank file =
      let clientPath = toClientPath root file.path
       in [ term "_" [iii| on click js window.open('#{clientPath}', '_blank'); end |] ]

    open file =
      let clientPath = toClientPath root file.path
          imgIdx = Maybe.fromJust $ Map.lookup file resourceIdxMap -- image index always exists
       in [ term "_" [iii| on click send Open(path: '#{clientPath}', index: #{imgIdx}) to window |] ]

    editor file =
      [ term "hx-get" "/modal/editor"
      , term "hx-vals" $ [ "file" .= toClientPath root file.path ] & toHxVals
      , term "hx-target" "#index"
      , term "hx-swap" "beforeend"
      ]

    resourceIdxMap :: Map File Int
    resourceIdxMap = Map.fromList $ Viewer.takeResourceFiles files `zip` [0..]


contextMenu :: FilePath -> File -> Html ()
contextMenu root file = do
  let textClientPath = toClientPath root file.path

  div_ [ class_ "dropdown-content "
       , id_ componentIds.contextMenu
       , term "_"
           [iii|
             init call htmx.process(me) end
             on Close
               add .closing
               then wait for animationend
               then remove me
             end

             on Show(pageX, pageY, path)
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
             , term "hx-get" ("/cd?dir=" <> textClientPath )
             , term "hx-target" ("#" <> componentIds.view)
             , term "hx-swap" "outerHTML"
             ] $
          span_ "Open"
      Content
        | file.mimetype `isMime` "application/pdf" -> do
          a_ [ class_ "dropdown-item"
             , href_ (URI.Encode.decodeText textClientPath)
             , target_ "blank"
             ] $
            span_ "View"
        | file.mimetype `isMime` "audio" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" ("/viewer?=" <> textClientPath)
               , term "hx-vals" $ [ "file" .= textClientPath ] & toHxVals
               , term "hx-target" "this"
               , term "hx-swap" "none"
               ] $
            span_ "Play"
        | file.mimetype `isMime` "video" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" ("/viewer?=" <> textClientPath)
               , term "hx-vals" $ [ "file" .= textClientPath ] & toHxVals
               , term "hx-target" "this"
               , term "hx-swap" "none"
               ] $
            span_ "Play"
        | file.mimetype `isMime` "image" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" ("/viewer?=" <> textClientPath)
               , term "hx-vals" $ [ "file" .= textClientPath ] & toHxVals
               , term "hx-target" "this"
               , term "hx-swap" "none"
               ] $
            span_ "View"
        | file.mimetype `isMime` "text" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" "/modal/editor"
               , term "hx-vals" $ [ "file" .= textClientPath ] & toHxVals
               , term "hx-target" "#index"
               , term "hx-swap" "beforeend"
               ] $
            span_ "Edit"
        | otherwise ->
            mempty
    div_ [ class_ "dropdown-item" ] $
      a_ [ href_ ("/download?file=" <> textClientPath ) ] "Download"

    div_ [ class_ "dropdown-item"
         , term "hx-delete" "/files/delete"
         , term "hx-vals" $ [ "file" .= textClientPath ] & toHxVals
         , term "hx-target" "#view"
         , term "hx-swap" "outerHTML"
         , term "hx-confirm" ("Are you sure about deleting " <> textClientPath <> "?")
         ] $
      span_ "Delete"

    div_ [ class_ "dropdown-item"
         , term "hx-get" "/modal/file/detail"
         , term "hx-vals" $ [ "file" .= textClientPath ] & toHxVals
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
  , sideBar :: Text
  , searchBar :: Text
  , pathBreadcrumb :: Text
  , table :: Text
  , newFileModal :: Text
  , newFolderModal :: Text
  , fileDetailModal :: Text
  , uploadModal :: Text
  , sortByDropdown :: Text
  , editorModal :: Text
  , contextMenu :: Text
  }
  deriving Show


componentIds :: ComponentIds
componentIds = ComponentIds
  { view = "view"
  , controlPanel = "control-panel"
  , sideBar = "side-bar"
  , searchBar = "search-bar"
  , pathBreadcrumb = "path-breadcrumb"
  , table = "table"
  , newFileModal = "new-file-modal"
  , newFolderModal = "new-folder-modal"
  , fileDetailModal = "file-detail-modal"
  , uploadModal = "upload-modal"
  , sortByDropdown = "sortby-dropdown"
  , editorModal = "editor-modal"
  , contextMenu = "contextmenu"
  }


------------------------------------
-- helpers
------------------------------------


toClientPath :: FilePath -> FilePath -> Text
toClientPath root p = Text.pack . (.unClientPath) $ Domain.toClientPath root p


toHxVals :: [Pair] -> Text
toHxVals xs = (xs & Aeson.object & Aeson.encode & LText.decodeUtf8) ^. strict
