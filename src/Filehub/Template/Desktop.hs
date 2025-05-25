{-# LANGUAGE NamedFieldPuns #-}

module Filehub.Template.Desktop
  ( index
  , sideBar
  , controlPanel
  , view
  , toolBar
  , newFileModal
  , newFolderModal
  , fileDetailModal
  , editorModal
  , search
  , contextMenu
  , table
  )
  where


import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as Maybe
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
import Filehub.Routes (Api(..))
import Filehub.Sort (sortFiles)
import Filehub.Mime (isMime)
import Filehub.Size (toReadableSize)
import Filehub.Selected qualified as Selected
import Filehub.ClientPath qualified as ClientPath
import Filehub.Viewer qualified as Viewer
import Filehub.Target (TargetView(..))
import Filehub.Target qualified as Target
import Filehub.Template.Internal (bold, toClientPath, viewId, tableId, sideBarId, searchBar)
import Filehub.Template.Internal qualified as Template
import Filehub.Links ( apiLinks, linkToText )
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Network.URI.Encode qualified as URI.Encode
import System.FilePath (takeFileName)
import Text.Fuzzy (simpleFilter)

------------------------------------
-- components
------------------------------------


index :: Bool
      -> Html ()
      -> Html ()
      -> ControlPanelState
      -> Html ()
index readOnly sideBar' view' controlPanelState = do
  div_ [ id_ "index" ] do
    sideBar'
    controlPanel readOnly controlPanelState
    view'


view :: Html () -> Html () -> Html ()
view table' pathBreadcrumb' = do
  div_ [ id_ viewId ] do
    toolBar pathBreadcrumb'
    table'


toolBar :: Html () -> Html ()
toolBar pathBreadcrumb' = do
  div_ [ id_ "tool-bar" ] do
    pathBreadcrumb'
    searchBar


sideBar :: [Target] -> TargetView -> Html ()
sideBar targets (TargetView currentTarget _ _) = do
  div_ [ id_ sideBarId ] do
    traverse_ targetIcon targets
  where
    targetIcon :: Target -> Html ()
    targetIcon target = do
      div_ [ class_ "target-icon"
           , term "hx-get" $ linkToText (apiLinks.changeTarget (Just (Target.getTargetId target)))
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


controlPanel :: Bool -> ControlPanelState -> Html ()
controlPanel =
  Template.controlPanel
    newFolderBtn
    newFileBtn
    uploadBtn
    copyBtn
    pasteBtn
    deleteBtn
    cancelBtn
    Nothing
  where
    newFolderBtn :: Html ()
    newFolderBtn =
      button_ [ class_ "btn btn-control "
              , type_ "submit"
              , term "hx-get" $ linkToText apiLinks.newFolderModal
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
              , term "hx-get" $ linkToText apiLinks.newFileModal
              , term "hx-target" "#index"
              , term "hx-swap" "beforeend"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-file-plus" ] mempty
          span_ "New File"


    uploadBtn :: Html ()
    uploadBtn = do
      let fileInputId = "file-input"
      input_ [ type_ "file"
             , name_ "file"
             , id_ fileInputId
             , style_ "display:none"
             , term "hx-encoding" "multipart/form-data"
             , term "hx-post" $ linkToText apiLinks.upload
             , term "hx-target" "#index"
             , term "hx-swap" "outerHTML"
             , term "hx-trigger" "change"
             ]

      button_ [ class_ "btn btn-control"
              , onclick_ [iii|document.querySelector('\##{fileInputId}').click()|]
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bx-upload" ] mempty
          span_ "Upload"


    copyBtn :: Html ()
    copyBtn = do
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-get" $ linkToText apiLinks.copy
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
              , term "hx-get" $ linkToText apiLinks.paste
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
              , term "hx-delete" $ linkToText (apiLinks.deleteFile Nothing True)
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
              , term "hx-post" $ linkToText apiLinks.cancel
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


------------------------------------
-- modals
------------------------------------


newFileModal :: Html ()
newFileModal = do
  modal [ id_ newFileModalId ] do
    bold "File"
    br_ mempty >> br_ mempty
    form_ [ term "hx-post" $ linkToText (apiLinks.newFile)
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
              ] "CLOSE"


newFolderModal :: Html ()
newFolderModal = do
  modal [ id_ newFolderModalId ] do
    bold "Folder"
    br_ mempty >> br_ mempty
    form_ [ term "hx-post" $ linkToText (apiLinks.newFolder)
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
  modal [ id_ fileDetailModalId ] do
    bold "Detail"
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


editorModal :: Bool -> FilePath -> LBS.ByteString -> Html ()
editorModal readOnly filename content = do

  modal [ id_ editorModalId ] do
    case readOnly of
      True -> bold "Read-only"
      False -> bold "Edit"

    form_ [ term "hx-put" $ linkToText (apiLinks.updateFile)
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
        (mconcat
          [
            [ class_ "form-control "
            , type_ "text"
            , name_ "content"
            , placeholder_ "Empty File"
            ]
          , if readOnly then [ readonly_ "readonly" ] else mempty
          ]
        )
        (toHtml $ LText.decodeUtf8 content)


      br_ mempty >> br_ mempty

      case readOnly of
        True -> do
          mempty
        False -> do
          button_ [ class_ "btn btn-modal-confirm mr-2 "
                  , term "_" "on click trigger Close"
                  ] "EDIT"

          button_ [ class_ "btn btn-modal-close "
                  , type_ "button"
                  , term "_" "on click trigger Close"
                  ] "CLOSE"


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
  table_ [ id_ tableId ] do
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
      [ term "hx-get" $ linkToText (apiLinks.sortTable (Just o))
      , term "hx-swap" "outerHTML"
      , term "hx-target" "#view"
      ]


    sizeElement :: File -> Html ()
    sizeElement file =
      span_ (toHtml displaySize)
        `with` [ class_ "field file-meta"
               , title_ (Text.pack displaySize)
               ]
      where
        displaySize = toReadableSize $ fromMaybe 0 file.size


    modifiedDateElement :: File -> Html ()
    modifiedDateElement file =
      span_ (toHtml displayTime)
        `with` [ class_ "field file-meta"
               , title_ (Text.pack displayTime)
               ]
      where
        displayTime = maybe mempty (formatTime defaultTimeLocale "%d %b %y") file.mtime


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
                    [ term "hx-get" $ linkToText (apiLinks.cd (Just (ClientPath.toClientPath root file.path)))
                    , term "hx-target" ("#" <> viewId)
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
      let ClientPath path = ClientPath.toClientPath root file.path
       in [ term "_" [iii| on click js window.open('/serve?file=#{path}', '_blank'); end |] ]


    open file =
      let ClientPath path = ClientPath.toClientPath root file.path
          imgIdx = Maybe.fromJust $ Map.lookup file resourceIdxMap -- image index always exists
       in [ term "_" [iii| on click send Open(path: '#{path}', index: #{imgIdx}) to body |] ]


    editor file =
      [ term "hx-get" $ linkToText (apiLinks.editorModal (Just (ClientPath.toClientPath root file.path)))
      , term "hx-target" "#index"
      , term "hx-swap" "beforeend"
      ]

    resourceIdxMap :: Map File Int
    resourceIdxMap = Map.fromList $ Viewer.takeResourceFiles files `zip` [0..]


contextMenu :: Bool -> FilePath -> File -> Html ()
contextMenu readOnly root file = do
  let textClientPath = toClientPath root file.path
  let clientPath = ClientPath.toClientPath root file.path

  div_ [ class_ "dropdown-content "
       , id_ contextMenuId
      ] do

    case file.content of
      Dir _ -> do
        div_ [ class_ "dropdown-item"
             , term "hx-get" $ linkToText (apiLinks.cd (Just clientPath))
             , term "hx-target" ("#" <> viewId)
             , term "hx-swap" "outerHTML"
             ] $
          span_ "Open"
      Content
        | file.mimetype `isMime` "application/pdf" -> do
          div_ [ class_ "dropdown-item" ] $
            a_ [ href_ (URI.Encode.decodeText textClientPath) , target_ "blank" ] "View"
        | file.mimetype `isMime` "audio" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" $ linkToText (apiLinks.initViewer (Just clientPath))
               , term "hx-target" "this"
               , term "hx-swap" "none"
               ] $
            span_ "Play"
        | file.mimetype `isMime` "video" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" $ linkToText (apiLinks.initViewer (Just clientPath))
               , term "hx-target" "this"
               , term "hx-swap" "none"
               ] $
            span_ "Play"
        | file.mimetype `isMime` "image" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" $ linkToText (apiLinks.initViewer (Just clientPath))
               , term "hx-target" "this"
               , term "hx-swap" "none"
               ] $
            span_ "View"
        | file.mimetype `isMime` "text" -> do
          div_ [ class_ "dropdown-item"
               , term "hx-get" $ linkToText (apiLinks.editorModal (Just clientPath))
               , term "hx-target" "#index"
               , term "hx-swap" "beforeend"
               ] $
            span_ "Edit"
        | otherwise ->
            mempty
    div_ [ class_ "dropdown-item" ] $
      a_ [ href_ (linkToText $ apiLinks.download (Just clientPath)) ] "Download"

    case readOnly of
      True -> mempty
      False -> do
        div_ [ class_ "dropdown-item"
             , term "hx-delete" $ linkToText (apiLinks.deleteFile (Just clientPath) False)
             , term "hx-target" "#index"
             , term "hx-swap" "outerHTML"
             , term "hx-confirm" ("Are you sure about deleting " <> textClientPath <> "?")
             ] $
          span_ "Delete"

    div_ [ class_ "dropdown-item"
         , term "hx-get" $ linkToText (apiLinks.fileDetailModal (Just clientPath))
         , term "hx-target" "#index"
         , term "hx-swap" "beforeend"
         ] $
      span_ "Details"

------------------------------------
-- component ids
------------------------------------


newFileModalId :: Text
newFileModalId = "new-file-modal"

newFolderModalId :: Text
newFolderModalId = "new-folder-modal"

fileDetailModalId :: Text
fileDetailModalId = "file-detail-modal"

editorModalId :: Text
editorModalId = "editor-modal"

contextMenuId :: Text
contextMenuId = "contextmenu"
