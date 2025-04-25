{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}

module Filehub.Template
  ( withDefault
  , index
  , sideBar
  , controlPanel
  , view
  , toolBar
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


import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.Bifunctor (Bifunctor(..))
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (traverse_, Foldable (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.String.Interpolate (iii, i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as LText
import Data.Time.Format (formatTime, defaultTimeLocale)
import Filehub.Types
    ( File(..),
      FileContent(..),
      SearchWord(..),
      SortFileBy(..),
      ClientPath(..),
      Target(..),
      S3Target(..),
      FileTarget(..),
      Selected,
      ControlPanelState(..) )
import Filehub.Sort (sortFiles)
import Filehub.Mime (isMime)
import Filehub.Size (toReadableSize)
import Filehub.Selected qualified as Selected
import Filehub.ClientPath qualified as ClientPath
import Filehub.Viewer qualified as Viewer
import Filehub.Target (TargetView(..))
import Filehub.Target qualified as Target
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Network.URI.Encode qualified as URI.Encode
import Servant (ToHttpApiData(..))
import System.FilePath (splitPath, takeFileName)
import Text.Fuzzy (simpleFilter)

------------------------------------
-- components
------------------------------------


index :: Html ()
      -> Html ()
      -> ControlPanelState
      -> Html ()
index sideBar' view' controlPanelState = do
  div_ [ id_ "index" ] do
    sideBar'
    controlPanel controlPanelState
    view'


controlPanel :: ControlPanelState -> Html ()
controlPanel state = do
  case state of
    ControlPanelDefault ->
      div_ [ id_ elementId ] do
        newFolderBtn
        newFileBtn
        uploadBtn
    ControlPanelSelecting ->
      div_ [ id_ elementId ] do
        newFolderBtn
        newFileBtn
        uploadBtn
        copyBtn
        deleteBtn
        cancelBtn
    ControlPanelCopied ->
      div_ [ id_ elementId ] do
        newFolderBtn
        newFileBtn
        uploadBtn
        pasteBtn
        cancelBtn
  where
    elementId = componentIds.controlPanel


view :: Html () -> Html () -> Html ()
view table' pathBreadcrumb' = do
  div_ [ id_ componentIds.view ] do
    toolBar pathBreadcrumb'
    table'


toolBar :: Html () -> Html ()
toolBar pathBreadcrumb' = do
  div_ [ id_ "tool-bar" ] do
    pathBreadcrumb'
    searchBar


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
      `with` tooltipInfo
      where
        targetAttr t = [class_ " current-target" | Target.getTargetId currentTarget == Target.getTargetId t]
        tooltipInfo =
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
      span_ "New Folder"


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
      span_ "New File"


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
      span_ "Upload"


copyBtn :: Html ()
copyBtn = do
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/files/copy"
          , term "hx-target" "#control-panel"
          , term "hx-swap" "outerHTML"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bxs-copy-alt" ] mempty
      span_ "Copy"


pasteBtn :: Html ()
pasteBtn = do
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/files/paste"
          , term "hx-target" "#index"
          , term "hx-swap" "outerHTML"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bxs-paste" ] mempty
      span_ "Paste"


deleteBtn :: Html ()
deleteBtn = do
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-delete" "/files/delete?selected"
          , term "hx-target" "#index"
          , term "hx-swap" "outerHTML"
          , term "hx-confirm" ("Are you sure about deleting selected files?")
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bxs-trash" ] mempty
      span_ "Delete"


cancelBtn :: Html ()
cancelBtn = do
  button_ [ class_ "btn btn-control"
          , type_ "submit"
          , term "hx-get" "/cancel"
          , term "hx-target" "#index"
          , term "hx-swap" "outerHTML"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bxs-message-alt-x" ] mempty
      span_ "Cancel"


------------------------------------
-- search
------------------------------------


search :: SearchWord -> Target -> FilePath -> [File] -> Selected -> SortFileBy -> Html ()
search (SearchWord searchWord) target root files selected order = do
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  table target root (sortFiles order filteredFiles) selected order


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
          td_ (toHtml . toReadableSize $ fromMaybe 0 file.size)
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


table :: Target -> FilePath -> [File] -> Selected -> SortFileBy -> Html ()
table target root files selected order = do
  table_ [ id_ componentIds.table ] do
    thead_ do
      tr_ do
        th_ do
          span_ [ class_ "field " ] do
            "Name "
            sortIconName
          `with` sortControlName
        th_ do
          span_ [ class_ "field " ] do
            "Modified"
            sortIconMTime
            `with` sortControlMTime
        th_ do
          span_ [ class_ "field " ] do
            "Size"
            sortIconSize
            `with` sortControlSize
    tbody_ $ traverse_ record ([0..] `zip` files)
  where
    record :: (Int, File) -> Html ()
    record (idx, file) =
      tr_ attrs do
        td_ $ fileNameElement file
        td_ $ modifiedDateElement file
        td_ $ sizeElement file
      where
        attrs :: [Attribute]
        attrs = mconcat
          [ [ term "data-path" (Text.pack path) ]
          , [class_ "selected " | clientPath `Selected.elem` selected]
          , [id_ [i|tr-#{idx}|] ]
          ]
        clientPath@(ClientPath path) = ClientPath.toClientPath root file.path


    sortIconName =
      case order of
        ByNameUp -> i_ [ class_ "bx bxs-up-arrow"] mempty
        ByNameDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
        _ -> i_ [ class_ "bx bx-sort"] mempty


    sortIconMTime =
      case order of
        ByModifiedUp -> i_ [ class_ "bx bxs-up-arrow"] mempty
        ByModifiedDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
        _ -> i_ [ class_ "bx bx-sort"] mempty


    sortIconSize =
      case order of
        BySizeUp -> i_ [ class_ "bx bxs-up-arrow"] mempty
        BySizeDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
        _ -> i_ [ class_ "bx bx-sort"] mempty


    sortControlName =
      case order of
        ByNameUp -> sortControl ByNameDown
        ByNameDown -> sortControl ByNameUp
        _ -> sortControl ByNameUp


    sortControlMTime =
      case order of
        ByModifiedUp -> sortControl ByModifiedDown
        ByModifiedDown -> sortControl ByModifiedUp
        _ -> sortControl ByModifiedUp


    sortControlSize =
      case order of
        BySizeUp -> sortControl BySizeDown
        BySizeDown -> sortControl BySizeUp
        _ -> sortControl BySizeUp


    sortControl o =
      [ term "hx-get" "/table/sort"
      , term "hx-vals" $ [ "by" .= toUrlPiece o ] & toHxVals
      , term "hx-swap" "outerHTML"
      , term "hx-target" "#view"
      ]


    sizeElement :: File -> Html ()
    sizeElement file =
      span_ (toHtml displaySize)
        `with` [ class_ "field "
               , title_ (Text.pack displaySize)
               ]
      where
        displaySize = toReadableSize $ fromMaybe 0 file.size


    modifiedDateElement :: File -> Html ()
    modifiedDateElement file =
      span_ (toHtml displayTime)
        `with` [ class_ "field "
               , title_ (Text.pack displayTime)
               ]
      where
        displayTime = maybe mempty (formatTime defaultTimeLocale "%F %R") file.mtime


    fileNameElement :: File -> Html ()
    fileNameElement file = do
      span_ (icon >> name)
        `with` [ class_ "field"
               , title_ (Text.pack displayName)
               ]
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

        icon =
          case file.content of
            Dir _ -> i_ [ class_ "bx bxs-folder "] mempty
            Content -> i_ [ class_ "bx bxs-file-blank "] mempty

        displayName =
          case target of
            S3Target _ -> file.path
            FileTarget _ -> takeFileName file.path


    openBlank file =
      -- Client path are percent encoded, but we need to use unencoded raw path here.
      let ClientPath clientPath = ClientPath.toClientPath root file.path
          path = URI.Encode.decode clientPath
       in [ term "_" [iii| on click js window.open('#{path}', '_blank'); end |] ]


    open file =
      let ClientPath clientPath = ClientPath.toClientPath root file.path
          path = URI.Encode.decode clientPath
          imgIdx = Maybe.fromJust $ Map.lookup file resourceIdxMap -- image index always exists
       in [ term "_" [iii| on click send Open(path: '#{path}', index: #{imgIdx}) to window |] ]


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
         , term "hx-target" "#index"
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
  , editorModal = "editor-modal"
  , contextMenu = "contextmenu"
  }


------------------------------------
-- helpers
------------------------------------


toClientPath :: FilePath -> FilePath -> Text
toClientPath root p = Text.pack . (.unClientPath) $ ClientPath.toClientPath root p


toHxVals :: [Pair] -> Text
toHxVals xs = (xs & Aeson.object & Aeson.encode & LText.decodeUtf8) ^. strict
