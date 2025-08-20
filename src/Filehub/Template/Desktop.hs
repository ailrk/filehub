{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}

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


import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (iii, i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Format (formatTime, defaultTimeLocale)
import Filehub.Types
    ( File(..),
      FileContent(..),
      SearchWord(..),
      SortFileBy(..),
      ClientPath(..),
      Target(..),
      Selected(..),
      ControlPanelState(..))
import Filehub.Routes (Api(..))
import Filehub.Sort (sortFiles)
import Filehub.Mime (isMime)
import Filehub.Size (toReadableSize)
import Filehub.Selected qualified as Selected
import Filehub.ClientPath qualified as ClientPath
import Filehub.Target (TargetView(..), handleTarget)
import Filehub.Target qualified as Target
import Filehub.Template.Internal (bold, toClientPath, viewId, tableId, sideBarId, searchBar)
import Filehub.Template.Internal qualified as Template
import Filehub.Links ( apiLinks, linkToText )
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import System.FilePath (takeFileName)
import Text.Fuzzy (simpleFilter)
import Filehub.Target.S3 (S3, Backend (..))
import Filehub.Target.File (FileSys, Backend (..))
import Filehub.Target.Types (targetHandler)
import Filehub.Layout (Layout (..))
import Filehub.Theme (Theme (..))
import Control.Monad (when)

------------------------------------
-- components
------------------------------------


index :: Bool
      -> Html ()
      -> Html ()
      -> Layout
      -> Theme
      -> ControlPanelState
      -> Html ()
index readOnly sideBar' view' layout theme controlPanelState = do
  div_ [ id_ "index" ] do
    sideBar'
    controlPanel layout theme readOnly controlPanelState
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


sideBar :: [(Target, Int)] -> TargetView -> Html ()
sideBar targets (TargetView currentTarget _ _) = do
  div_ [ id_ sideBarId ] do
    traverse_ targetTab targets
  where
    targetTab :: (Target, Int) -> Html ()
    targetTab (target, selectedCount) = do
      div_ [ class_ "target-tab"
           , term "hx-get" $ linkToText (apiLinks.changeTarget (Just (Target.getTargetId target)))
           , term "hx-target" "#index"
           , term "hx-swap" "outerHTML"
           ] do
        span_ [ class_ "field "] do
          fromMaybe "unknown" $ handleTarget target
            [ targetHandler @S3 . const $ i_ [ class_ "bx bxs-cube" ] mempty
            , targetHandler @FileSys . const $ i_ [ class_ "bx bx-folder" ] mempty
            ]

          fromMaybe "" $ handleTarget target
            [ targetHandler @S3 $ \(S3Backend { bucket }) -> span_ [iii| /#{bucket} |]
            , targetHandler @FileSys $ \(FileBackend { root }) -> span_ [iii| /#{takeFileName root} |]
            ]

        when (selectedCount > 0) do
          div_ [ class_ "target-tab-selected-counter" ] do
            toHtml . Text.pack . show $ selectedCount

      `with` targetAttr target
      `with` tooltipInfo
      where
        targetAttr t = [class_ " current-target" | Target.getTargetId currentTarget == Target.getTargetId t]
        tooltipInfo =
          fromMaybe [] $ handleTarget target
            [ targetHandler @S3 $ \(S3Backend { bucket }) ->
                [ term "data-target-info" [iii| [S3] #{bucket} |] ]
            , targetHandler @FileSys $ \(FileBackend { root }) ->
                [ term "data-target-info" [iii| [FileSystem] #{takeFileName root} |] ]
            ]


controlPanel :: Layout -> Theme -> Bool -> ControlPanelState -> Html ()
controlPanel layout theme =
  Template.controlPanel
    newFolderBtn
    newFileBtn
    uploadBtn
    copyBtn
    pasteBtn
    deleteBtn
    cancelBtn
    themeBtn
    logoutBtn
    (Just layoutBtn)
    Nothing
  where
    newFolderBtn :: Html ()
    newFolderBtn =
      button_ [ class_ "btn btn-control "
              , type_ "submit"
              , term "hx-get" $ linkToText apiLinks.newFolderModal
              , term "hx-target" "#index"
              , term "hx-swap" "beforeend"
              , term "data-btn-title" "New folder"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bx-folder-plus" ] mempty


    newFileBtn :: Html ()
    newFileBtn  =
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-get" $ linkToText apiLinks.newFileModal
              , term "hx-target" "#index"
              , term "hx-swap" "beforeend"
              , term "data-btn-title" "New file"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-file-plus" ] mempty


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
              , term "data-btn-title" "Upload"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bx-upload" ] mempty

    copyBtn :: Html ()
    copyBtn = do
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-get" $ linkToText apiLinks.copy
              , term "hx-target" "#control-panel"
              , term "hx-swap" "outerHTML"
              , term "data-btn-title" "Copy"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-copy-alt" ] mempty


    pasteBtn :: Html ()
    pasteBtn = do
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-post" $ linkToText apiLinks.paste
              , term "hx-target" "#index"
              , term "hx-swap" "outerHTML"
              , term "data-btn-title" "Paste"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-paste" ] mempty


    deleteBtn :: Html ()
    deleteBtn = do
      button_ [ class_ "btn btn-control urgent"
              , type_ "submit"
              , term "hx-delete" $ linkToText (apiLinks.deleteFile [] True)
              , term "hx-target" "#index"
              , term "hx-swap" "outerHTML"
              , term "hx-confirm" "Are you sure about deleting selected files?\n\n (All selected files will be deleted)"
              , term "data-btn-title" "Delete"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-trash" ] mempty


    cancelBtn :: Html ()
    cancelBtn = do
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-post" $ linkToText apiLinks.cancel
              , term "hx-target" "#index"
              , term "hx-swap" "outerHTML"
              , term "data-btn-title" "Cancel"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-message-alt-x" ] mempty


    logoutBtn :: Html ()
    logoutBtn = do
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-post" $ linkToText apiLinks.logout
              , term "hx-target" "#index"
              , term "hx-swap" "outerHTML"
              , term "hx-confirm" "Logout?"
              , term "data-btn-title" "Logout"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bx-power-off" ] mempty


    themeBtn :: Html ()
    themeBtn = do
      case theme of
        Light -> do
          button_ [ class_ "btn btn-control"
                  , type_ "submit"
                  , term "hx-get" $ linkToText apiLinks.toggleTheme
                  , term "hx-target" "#index"
                  , term "hx-swap" "outerHTML"
                  , term "data-btn-title" "Dark"
                  ] do
            i_ [ class_ "bx bxs-moon" ] mempty
        Dark -> do
          button_ [ class_ "btn btn-control"
                  , type_ "submit"
                  , term "hx-get" $ linkToText apiLinks.toggleTheme
                  , term "hx-target" "#index"
                  , term "hx-swap" "outerHTML"
                  , term "data-btn-title" "Light"
                  ] do
            i_ [ class_ "bx bxs-sun" ] mempty


    layoutBtn :: Html ()
    layoutBtn = do
      case layout of
        ListLayout -> do
          button_ [ class_ "btn btn-control"
                  , type_ "submit"
                  , term "hx-get" $ linkToText (apiLinks.selectLayout (Just ThumbnailLayout))
                  , term "hx-target" "#index"
                  , term "hx-swap" "outerHTML"
                  , term "data-btn-title" "Grid"
                  ] do
            i_ [ class_ "bx bxs-grid-alt" ] mempty
        ThumbnailLayout -> do
          button_ [ class_ "btn btn-control"
                  , type_ "submit"
                  , term "hx-get" $ linkToText (apiLinks.selectLayout (Just ListLayout))
                  , term "hx-target" "#index"
                  , term "hx-swap" "outerHTML"
                  , term "data-btn-title" "List"
                  ] do
            i_ [ class_ "bx bx-menu" ] mempty


------------------------------------
-- search
------------------------------------


search :: SearchWord -> Target -> FilePath -> [File] -> Selected -> SortFileBy -> Layout -> Html ()
search (SearchWord searchWord) target root files selected order layout = do
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  table target root (sortFiles order filteredFiles) selected order layout


------------------------------------
-- modals
------------------------------------


newFileModal :: Html ()
newFileModal = do
  modal [ id_ newFileModalId ] do
    span_ [ class_ "modal-title-bar " ] do
      bold "Folder"
      div_ [ class_ "title-bar-btn btn-modal-close "
           , term "_" "on click trigger Close"
           ] do
        i_ [ class_ "bx bx-x"] mempty
    br_ mempty
    form_ [ term "hx-post" $ linkToText (apiLinks.newFile)
          , term "hx-target" "#view"
          , term "hx-swap" "outerHTML"
          ] do
      div_ [ style_ "display: flex" ] do
        input_ [ class_ "form-control "
               , type_ "text"
               , name_ "new-file"
               , placeholder_ "New file name"
               ]
        button_ [ class_ "btn btn-modal-confirm "
                , type_ "submit"
                , term "_" "on click trigger Close"
                ] "CREATE"


newFolderModal :: Html ()
newFolderModal = do
  modal [ id_ newFolderModalId ] do
    span_ [ class_ "modal-title-bar " ] do
      bold "Folder"
      div_ [ class_ "title-bar-btn btn-modal-close "
           , term "_" "on click trigger Close"
           ] do
        i_ [ class_ "bx bx-x"] mempty
    br_ mempty
    form_ [ term "hx-post" $ linkToText (apiLinks.newFolder)
          , term "hx-target" "#view"
          , term "hx-swap" "outerHTML"
          ] do
      div_ [ style_ "display: flex" ] do
        input_ [ class_ "form-control "
               , type_ "text"
               , name_ "new-folder"
               , placeholder_ "New folder name"
               ]
        button_ [ class_ "btn btn-modal-confirm "
                , type_ "submit"
                , term "_" "on click trigger Close"
                ] "CREATE"


fileDetailModal :: File -> Html ()
fileDetailModal file = do
  modal [ id_ fileDetailModalId ] do
    bold "Detail"
    br_ mempty
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


editorModal :: Bool -> FilePath -> ByteString -> Html ()
editorModal readOnly filename content = do
  modal [ id_ editorModalId ] do

    case readOnly of
      True -> bold "Read-only"
      False ->  do
        span_ [ class_ "modal-title-bar " ] do
          bold "Edit"
          div_ [ class_ "title-bar-btn btn-modal-close "
               , term "_" "on click trigger Close"
               ] do
            i_ [ class_ "bx bx-x"] mempty

    br_ mempty

    form_ [ term "hx-post" $ linkToText (apiLinks.updateFile)
          , term "hx-confirm" ("Save the edit of " <> Text.pack filename <> "?")
          ] do
      input_ [ class_ "form-control "
             , type_ "text"
             , name_ "path"
             , value_ (Text.pack filename)
             , placeholder_ "Filename"
             ]

      br_ mempty
      br_ mempty

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
        (toHtml $ Text.decodeUtf8 content)

      br_ mempty >> br_ mempty

      case readOnly of
        True -> do
          mempty
        False -> do
          button_ [ class_ "btn btn-modal-confirm-1 "
                  , term "_" "on click trigger Close"
                  ] "EDIT"


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


table :: Target -> FilePath -> [File] -> Selected -> SortFileBy -> Layout -> Html ()
table target root files selected order layout =
  case layout of
    ListLayout -> listLayout
    ThumbnailLayout -> thumbnailLayout
  where
    listLayout = do
      table_ [ id_ tableId, class_ "list-view " ] do
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
        record (idx, file) = do
          let clientPath@(ClientPath path) = ClientPath.toClientPath root file.path
          tr_ do
            td_ $ fileNameElement file True
                    `with` Template.open root file
                    `with`  [ class_ "field "]
            td_ $ modifiedDateElement file
            td_ $ sizeElement file
            `with`
              mconcat
                [ [ term "data-path" (Text.pack path) ]
                , [ class_ "selected " | clientPath `Selected.elem` selected]
                , [ id_ [i|tr-#{idx}|]
                  , class_ "table-item "
                  , draggable_ "true"
                  ]
                , case file.content of Dir _ -> [ class_ "dir "]; _ -> mempty
                ]


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


    thumbnailLayout = do
      div_ [ id_ tableId, class_ "thumbnail-view " ] do
        tbody_ $ traverse_ thumbnail ([0..] `zip` files)
      where
        thumbnail :: (Int, File) -> Html ()
        thumbnail (idx, file) = card `with` Template.open root file
          where
            card = div_ do
              previewElement file
              fileNameElement file False `with` [ class_ "thumbnail-name" ]
              `with`
                mconcat
                  [ [ term "data-path" (Text.pack path) ]
                  , [ class_ "selected " | clientPath `Selected.elem` selected ]
                  , [ id_ [i|tr-#{idx}|]
                    , class_ "thumbnail table-item "
                    , draggable_ "true"
                    ]
                  , case file.content of Dir _ -> [ class_ "dir "]; _ -> mempty
                  ]

            clientPath@(ClientPath path) = clientPathOf file


    previewElement :: File -> Html ()
    previewElement file = do
      div_ [ class_ "thumbnail-preview " ] do
        div_ [  class_ "image-wrapper " ] do
          if
             | file.mimetype `isMime` "image" ->
               img_ [ loading_ "lazy"
                    , src_ (linkToText $ apiLinks.thumbnail (Just $ clientPathOf file))
                    , draggable_ "false"
                    ]
             | otherwise -> Template.icon file


    fileNameElement :: File -> Bool -> Html ()
    fileNameElement file withIcon = do
      span_ ((if withIcon then Template.icon file else mempty) >> name)
        `with` [ title_ (Text.pack displayName) ]
      where
        name = span_ (toHtml displayName)

        displayName =
          fromMaybe "-" $ handleTarget target
            [ targetHandler @S3 $ \_ -> file.path
            , targetHandler @FileSys $ \_ -> takeFileName file.path
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
        displayTime = maybe mempty (formatTime defaultTimeLocale "%Y/%m/%d") file.mtime


    clientPathOf :: File -> ClientPath
    clientPathOf file = ClientPath.toClientPath root file.path


contextMenu :: Bool -> FilePath -> [File] -> Html ()
contextMenu readOnly root [file] = do
  let textClientPath = toClientPath root file.path
  let clientPath = ClientPath.toClientPath root file.path

  div_ [ class_ "dropdown-content " , id_ contextMenuId ] do
    case file.content of
      Dir _ -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bxs-folder-open" ] mempty >> span_ "Open"
      Content
        | file.mimetype `isMime` "application/pdf" -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bx-show" ] mempty >> span_ "View"
        | file.mimetype `isMime` "audio" -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bx-play" ] mempty >> span_ "Play"
        | file.mimetype `isMime` "video" -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bx-play" ] mempty >> span_ "Play"
        | file.mimetype `isMime` "image" -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bx-show" ] mempty >> span_ "View"
        | file.mimetype `isMime` "text" -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bxs-edit" ] mempty >> span_ "Edit"
        | otherwise -> mempty
      `with` Template.open root file

    a_ [ class_ "dropdown-item" ,  href_ (linkToText $ apiLinks.download [clientPath]) ] do
      i_ [ class_ "bx bx-download" ] mempty
      span_ "Download"

    case readOnly of
      True -> mempty
      False -> do
        div_ [ class_ "dropdown-item"
             , term "hx-delete" $ linkToText (apiLinks.deleteFile [clientPath] False)
             , term "hx-target" "#index"
             , term "hx-swap" "outerHTML"
             , term "hx-confirm" ("Are you sure about deleting " <> textClientPath <> "?")
             ] do
          i_ [ class_ "bx bxs-trash" ] mempty
          span_ "Delete"

    div_ [ class_ "dropdown-item"
         , term "hx-get" $ linkToText (apiLinks.fileDetailModal (Just clientPath))
         , term "hx-target" "#index"
         , term "hx-swap" "beforeend"
         ] do
      i_ [ class_ "bx bx-detail" ] mempty
      span_ "Details"
contextMenu readOnly root files = do
  let clientPaths = fmap (ClientPath.toClientPath root . (.path)) files

  div_ [ class_ "dropdown-content " , id_ contextMenuId ] do
    div_ [ class_ "dropdown-item no-effect" ] do
      i_ [ class_ "bx bx-select-multiple" ] mempty
      span_ [i|#{length files} Selected|]

    br_ []
    case readOnly of
      True -> mempty
      False -> do
        div_ [ class_ "dropdown-item"
             , term "hx-delete" $ linkToText (apiLinks.deleteFile clientPaths False)
             , term "hx-target" "#index"
             , term "hx-swap" "outerHTML"
             , term "hx-confirm" ("Are you sure about deleting " <> Text.pack (show (length clientPaths)) <> " files?\n\n (Only selected files in the current directory will be deleted)")
             ] do
          i_ [ class_ "bx bxs-trash" ] mempty
          span_ "Delete (Local)"

        div_ [ class_ "dropdown-item"
             , term "hx-get" $ linkToText apiLinks.copy
             , term "hx-target" "#control-panel"
             , term "hx-swap" "outerHTML"
             ] do
          i_ [ class_ "bx bx-detail" ] mempty
          span_ "Copy"


    a_ [ class_ "dropdown-item" ,  href_ (linkToText $ apiLinks.download clientPaths) ] do
      i_ [ class_ "bx bx-download" ] mempty
      span_ "Download"


    div_ [ class_ "dropdown-item"
         , term "hx-post" $ linkToText apiLinks.cancel
         , term "hx-target" "#index"
         , term "hx-swap" "outerHTML"
         ] do
      i_ [ class_ "bx bx-message-alt-x" ] mempty
      span_ "Cancel"



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
