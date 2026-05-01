{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Filehub.Template.Desktop
  ( index
  , sideBar
  , controlPanel
  , view
  , toolBar
  , entry
  , entries
  , thumbnail
  , renameModal
  , newFileModal
  , newFolderModal
  , fileDetailModal
  , editorModal
  , contextMenu1
  , contextMenuMany
  , table
  )
  where

import Control.Monad (when, join)
import Control.Monad.Reader (asks, MonadReader (..))
import Data.ByteString (ByteString)
import Data.ClientPath (ClientPath(..), AbsPath (..), Root (..))
import Data.ClientPath qualified as ClientPath
import Data.ClientPath.View (ClientPathView(..), AsClientPathView (..))
import Data.Coerce (coerce)
import Data.File (File(..), FileType(..), FileInfo, IsLink (..))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (iii, i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Format (formatTime, defaultTimeLocale)
import Filehub.Links ( apiLinks )
import Filehub.Locale (Locale(..), Phrase (..), phrase)
import Filehub.Routes (Api(..))
import Filehub.Session (TargetView(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (Layout(..))
import Filehub.Size (toReadableSize)
import Filehub.Template (Template, TemplateContext(..), runTemplate)
import Filehub.Template.Shared (bold, sideBarId, viewId, searchBar, tableId)
import Filehub.Template.Shared qualified as Template
import Filehub.Theme (Theme (..))
import Filehub.Types (SortFileBy(..))
import Lens.Micro.Platform ()
import Lucid
import Lucid.Htmx (hxGet, hxTarget, hxSwap, Swap (..), hxEncoding, hxPost, hxTrigger, Trigger (..), hxDelete, hxConfirm)
import Network.Mime.Extended (isMime)
import Servant.Extended (linkToText)
import System.FilePath (takeFileName)
import Target.Dummy (DummyTarget)
import Target.File (FileSys, Target (..))
import Target.S3 (S3, Target (..))
import Target.Types (targetHandler, AnyTarget, handleTarget)
import Target.Types qualified as Target


------------------------------------
-- components
------------------------------------


index :: Html ()
      -> Html ()
      -> Html ()
      -> Template (Html ())
index sideBar' view' toolBar' = do
  controlPanel' <- controlPanel
  sidebarCollapsed <- asks (.sidebarCollapsed)
  pure do
    div_ [ id_ "index" ] do
      sideBar'
      controlPanel'
      toolBar'
      view'
      `with`
      if sidebarCollapsed
         then [ class_ "sidebar-collapsed " ]
         else []


view :: Html () -> Html ()
view table' = do
  div_ [ id_ viewId ] do
    table'


toolBar :: Template (Html ())
toolBar = do
  pathBreadcrumb' <- Template.pathBreadcrumb
  searchBar' <- searchBar
  pure do
    div_ [ id_ "tool-bar" ] do
      pathBreadcrumb'
      searchBar'


sideBar :: [(AnyTarget, Int)] -> TargetView -> Template (Html ())
sideBar targets (TargetView currentTarget _) = do
  p <- phrase <$> asks (.locale)
  pure do
    div_ [ id_ sideBarId ] do
      traverse_ (targetTab p) targets
  where
    targetTab :: Phrase -> (AnyTarget, Int) -> Html ()
    targetTab Phrase { target_filesystem, target_s3 } (target, selectedCount) = do
      div_ [ class_ "target-tab"
           , hxGet (apiLinks.changeTarget (Just (Target.getTargetId target)))
           , hxTarget "#index"
           , hxSwap OuterHTML
           ] do
        span_ [ class_ "field "] do
          fromMaybe "unknown" $ handleTarget target
            [ targetHandler @S3      \_ -> i_ [ class_ "bx bxs-cube" ] mempty
            , targetHandler @FileSys \_ -> i_ [ class_ "bx bx-folder" ] mempty
            ]

          fromMaybe "" $ handleTarget target
            [ targetHandler @S3      \(S3Backend { bucket }) -> span_ [iii| /#{bucket} |]
            , targetHandler @FileSys \(FileBackend { root = Root (AbsPath root) }) -> span_ [iii| /#{takeFileName root} |]
            ]

        when (selectedCount > 0) do
          div_ [ class_ "target-tab-selected-counter" ] do
            (toHtml . Text.pack . show) selectedCount

      `with` targetAttr target
      `with` tooltipInfo
      where
        targetAttr t = [class_ " current-target" | Target.getTargetId currentTarget == Target.getTargetId t]
        tooltipInfo =
          fromMaybe [] $ handleTarget target
            [ targetHandler @S3 \(S3Backend { bucket }) ->
                [ term "data-target-info" [iii| [#{target_s3}] #{bucket} |] ]
            , targetHandler @FileSys \(FileBackend { root = Root (AbsPath root) }) ->
                [ term "data-target-info" [iii| [#{target_filesystem}] #{takeFileName root} |] ]
            ]


controlPanel :: Template (Html ())
controlPanel = join do
  Template.controlPanel
    <$> pure localeBtn
    <*> newFolderBtn
    <*> newFileBtn
    <*> uploadBtn
    <*> copyBtn
    <*> pasteBtn
    <*> deleteBtn
    <*> cancelBtn
    <*> themeBtn
    <*> logoutBtn
    <*> (Just <$> sortBtn)
    <*> (Just <$> toggleSidebarBtn)
    <*> (Just <$> layoutBtn)
    <*> pure Nothing


newFolderBtn :: Template (Html ())
newFolderBtn = do
  Phrase { control_panel_new_folder } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control "
            , type_ "submit"
            , hxGet apiLinks.newFolderModal
            , hxTarget "#index"
            , hxSwap BeforeEnd
            , term "data-btn-title" control_panel_new_folder
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-folder-plus" ] mempty


newFileBtn :: Template (Html ())
newFileBtn = do
  Phrase { control_panel_new_file } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control"
            , type_ "submit"
            , hxGet apiLinks.newFileModal
            , hxTarget "#index"
            , hxSwap BeforeEnd
            , term "data-btn-title" control_panel_new_file
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-file-plus" ] mempty


uploadBtn :: Template (Html ())
uploadBtn = do
  Phrase { control_panel_upload } <- phrase <$> asks (.locale)
  pure do
    let fileInputId = "file-input"
    input_ [ type_ "file"
           , name_ "file"
           , id_ fileInputId
           , style_ "display:none"
           , multiple_ ""
           , hxEncoding "multipart/form-data"
           , hxPost apiLinks.upload
           , hxTarget "#index"
           , hxSwap OuterHTML
           , hxTrigger Change
           ]

    button_ [ class_ "btn btn-control"
            , onclick_ [iii|document.querySelector('\##{fileInputId}').click()|]
            , term "data-btn-title" control_panel_upload
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-upload" ] mempty


copyBtn :: Template (Html ())
copyBtn = do
  Phrase { control_panel_copy } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control"
            , type_ "submit"
            , hxGet apiLinks.copy
            , hxTarget "#control-panel"
            , hxSwap OuterHTML
            , term "data-btn-title" control_panel_copy
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-copy-alt" ] mempty


pasteBtn :: Template (Html ())
pasteBtn = do
  Phrase { control_panel_paste } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control"
            , type_ "submit"
            , hxPost apiLinks.paste
            , hxSwap None
            , term "data-btn-title" control_panel_paste
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-paste" ] mempty


deleteBtn :: Template (Html ())
deleteBtn = do
  selected <- asks (.selected)
  Phrase
    { control_panel_delete
    , confirm_delete_all
    } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control urgent"
            , type_ "submit"
            , hxDelete (apiLinks.delete (Selected.toList selected) True)
            , hxSwap None
            , hxConfirm confirm_delete_all
            , term "data-btn-title" control_panel_delete
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-trash" ] mempty


cancelBtn :: Template (Html ())
cancelBtn = do
  Phrase { control_panel_cancel } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control"
            , type_ "submit"
            , hxPost apiLinks.cancel
            , hxTarget "#index"
            , hxSwap OuterHTML
            , term "data-btn-title" control_panel_cancel
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-message-alt-x" ] mempty


toggleSidebarBtn :: Template (Html ())
toggleSidebarBtn = do
  Phrase { toggle_sidebar } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control"
            , type_ "submit"
            , hxGet apiLinks.toggleSidebar
            , hxTarget "#index"
            , hxSwap OuterHTML
            , term "data-btn-title" toggle_sidebar
            ] do
      i_ [ class_ "bx bx-sidebar" ] mempty


logoutBtn :: Template (Html ())
logoutBtn = do
  Phrase { confirm_logout } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control urgent "
            , type_ "submit"
            , hxPost apiLinks.logout
            , hxTarget "#index"
            , hxSwap OuterHTML
            , hxConfirm confirm_logout
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-power-off" ] mempty


themeBtn :: Template (Html ())
themeBtn = do
  Phrase { control_panel_dark, control_panel_light } <- phrase <$> asks (.locale)
  theme <- asks (.theme)
  pure do
    case theme of
      Light -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , hxGet apiLinks.toggleTheme
                , hxTarget "#index"
                , hxSwap OuterHTML
                , term "data-btn-title" control_panel_dark
                ] do
          i_ [ class_ "bx bxs-moon" ] mempty
      Dark -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , hxGet apiLinks.toggleTheme
                , hxTarget "#index"
                , hxSwap OuterHTML
                , term "data-btn-title" control_panel_light
                ] do
          i_ [ class_ "bx bxs-sun" ] mempty


layoutBtn :: Template (Html ())
layoutBtn =  do
  layout <- asks (.layout)
  Phrase { control_panel_grid, control_panel_list } <- phrase <$> asks (.locale)
  pure do
    case layout of
      ListLayout -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , hxGet (apiLinks.selectLayout (Just ThumbnailLayout))
                , hxTarget "#index"
                , hxSwap OuterHTML
                , term "data-btn-title" control_panel_grid
                ] do
          i_ [ class_ "bx bxs-grid-alt" ] mempty
      ThumbnailLayout -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , hxGet (apiLinks.selectLayout (Just ListLayout))
                , hxTarget "#index"
                , hxSwap OuterHTML
                , term "data-btn-title" control_panel_list
                ] do
          i_ [ class_ "bx bx-menu" ] mempty



localeBtn :: Html ()
localeBtn =
  div_ [ class_ "control-panel-dropdown-btn" ] do
    button_ [ class_ "btn btn-control " ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-world" ] mempty
    div_ [ class_ "dropdown-content " ] do
      let item :: Locale -> Html () -> Html ()
          item loc label = div_ [ class_ "dropdown-item"
                                , hxGet (apiLinks.changeLocale (Just loc))
                                , hxTarget "#index"
                                , hxSwap OuterHTML
                                ] do span_ label
      item EN    "English"
      item ZH_CN "简体中文"
      item ZH_TW "繁體中文"
      item ZH_HK "繁體中文"
      item JA    "日本語"
      item ES    "Español"
      item FR    "Français"
      item DE    "Deutsch"
      item KO    "한국어"
      item RU    "Русский"
      item PT    "Português"
      item IT    "Italiano"


------------------------------------
-- modals
------------------------------------


newFileModal :: Template (Html ())
newFileModal = do
  Phrase
    { modal_file
    , modal_create
    , placeholder_newfile
    } <- phrase <$> asks (.locale)
  pure do
    modal [ id_ newFileModalId ] do
      span_ [ class_ "modal-title-bar " ] do
        bold (toHtml modal_file)
        div_ [ class_ "title-bar-btn btn-modal-close "
             , term "_" "on click trigger Close"
             ] do
          i_ [ class_ "bx bx-x"] mempty
      br_ mempty
      form_ [ hxPost apiLinks.newFile
            , hxSwap None
            ] do
        div_ [ style_ "display: flex" ] do
          input_ [ class_ "form-control "
                 , type_ "text"
                 , name_ "new-file"
                 , placeholder_ placeholder_newfile
                 ]
          button_ [ class_ "btn btn-modal-confirm "
                  , type_ "submit"
                  , term "_" "on click trigger Close"
                  ] (toHtml modal_create)




newFolderModal :: Template (Html ())
newFolderModal = do
  Phrase
    { modal_create
    , modal_folder
    , placeholder_newfoler
    } <- phrase <$> asks (.locale)
  pure do
    modal [ id_ newFolderModalId ] do
      span_ [ class_ "modal-title-bar " ] do
        bold (toHtml modal_folder)
        div_ [ class_ "title-bar-btn btn-modal-close "
             , term "_" "on click trigger Close"
             ] do
          i_ [ class_ "bx bx-x"] mempty
      br_ mempty
      form_ [ hxPost (apiLinks.newFolder)
            , hxSwap None
            ] do
        div_ [ style_ "display: flex" ] do
          input_ [ class_ "form-control "
                 , type_ "text"
                 , name_ "new-folder"
                 , placeholder_ placeholder_newfoler
                 ]
          button_ [ class_ "btn btn-modal-confirm "
                  , type_ "submit"
                  , term "_" "on click trigger Close"
                  ] (toHtml modal_create)


renameModal :: AbsPath -> Template (Html ())
renameModal oldPath = do
  root <- asks (.root)
  Phrase { modal_confirm } <- phrase <$> asks (.locale)

  let c2t           = Text.pack . coerce
      fileName      = Text.pack (coerce takeFileName oldPath)
      oldClientPath = ClientPath.toClientPath root oldPath

  pure do
    modal [ id_ renameModalId ] do
      form_ [ hxPost apiLinks.rename
            , hxTarget "#view"
            , hxSwap OuterHTML
            ] do
        input_ [ type_ "hidden", name_ "old", value_ (c2t oldClientPath) ]
        div_ [ style_ "display: flex" ] do
          input_ [ class_ "form-control "
                 , type_ "text"
                 , name_ "new"
                 , value_ fileName
                 ]
          button_ [ class_ "btn btn-modal-confirm "
                  , type_ "submit"
                  , term "_" "on click trigger Close"
                  ] (toHtml modal_confirm)


fileDetailModal :: FileInfo -> Template (Html ())
fileDetailModal file = do
  Phrase
    { modal_detail
    , detail_filename
    , detail_modified
    , detail_accessed
    , detail_size
    , detail_content_type
    } <- phrase <$> asks (.locale)

  pure do
    modal [ id_ fileDetailModalId ] do
      bold (toHtml modal_detail)
      br_ mempty
      table_ do
        tbody_ do
          tr_ do
            td_ (toHtml detail_filename)
            td_ (toHtml (takeFileName (coerce file.path)))
          tr_ do
            td_ (toHtml detail_modified)
            td_ (toHtml (maybe mempty (formatTime defaultTimeLocale "%F %R") file.mtime))
          tr_ do
            td_ (toHtml detail_accessed)
            td_ (toHtml (maybe mempty (formatTime defaultTimeLocale "%F %R") file.atime))
          tr_ do
            td_ (toHtml detail_size)
            td_ ((toHtml . toReadableSize) (fromMaybe 0 file.size))
          tr_ do
            td_ (toHtml detail_content_type)
            td_ (toHtml file.mimetype)


editorModal :: (ClientPath, String) -> ByteString -> Template (Html ())
editorModal (ClientPath path, filename) content = do
  readOnly <- asks (.readOnly)
  Phrase
    { modal_edit
    , modal_readonly
    , placeholder_empty_file
    , confirm_save_edit
    } <- phrase <$> asks (.locale)

  pure do
    modal [ id_ editorModalId ] do

      case readOnly of
        True -> bold (toHtml modal_readonly)
        False ->  do
          span_ [ class_ "modal-title-bar " ] do
            bold (toHtml modal_edit)
            bold (toHtml filename)
            div_ [ class_ "title-bar-btn btn-modal-close "
                 , term "_" "on click trigger Close"
                 ] do
              i_ [ class_ "bx bx-x"] mempty

      br_ mempty

      form_ [ hxPost (apiLinks.updateFile)
            , hxConfirm (Text.replace "{}" (Text.pack filename) confirm_save_edit)
            ] do
        input_ [ class_ "form-control ", type_ "hidden", name_ "path", value_ (Text.pack path) ]

        textarea_
          (mconcat
            [
              [ class_ "form-control "
              , type_ "text"
              , name_ "content"
              , placeholder_ placeholder_empty_file
              ]
            , if readOnly then [ readonly_ "readonly" ] else mempty
            ]
          )
          (toHtml (Text.decodeUtf8 content))

        br_ mempty >> br_ mempty

        case readOnly of
          True -> do
            mempty
          False -> do
            button_ [ class_ "btn btn-modal-confirm-1 "
                    , term "_" "on click trigger Close"
                    ] (toHtml modal_edit)


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


-----------------------------------------------------------------------------------------
-- table
--
-- == Item selection and `.selected` `.confirmed`.
--   Each session maintains a list of selected items. When generating the table, a selected item
--   should have `.selected` `.confirmed` attached to it.
--   `.selected.confirmed` proves that the frontend state is in sync with the backend. Meanwhile a
--   single `.selected` can mean an item is selected in the frontend, but not yet confirmed.
--   Isolated `.confirmed` is not defined and can indicate a bug.
-----------------------------------------------------------------------------------------


table :: [FileInfo] ->  Template (Html ())
table files = do
  layout <- asks (.layout)
  case layout of
    ListLayout      -> listLayout files
    ThumbnailLayout -> thumbnailLayout files


listLayout :: [FileInfo]  -> Template (Html ())
listLayout files = do
  Phrase
    { detail_filename
    , detail_modified
    , detail_size
    } <- phrase <$> asks (.locale)
  order <- asks (.sortedBy)
  ctx   <- ask

  pure do
    table_ [ id_ tableId, class_ "list-view " ] do
      thead_ do
        tr_ do
          th_ do
            span_ [ class_ "field " ] do
              (toHtml detail_filename)
              sortIconName order
            `with` sortControlName order
          th_ do
            span_ [ class_ "field " ] do
              (toHtml detail_modified)
              sortIconMTime order
              `with` sortControlMTime order
          th_ do
            span_ [ class_ "field " ] do
              (toHtml detail_size)
              sortIconSize order
              `with` sortControlSize order
      tbody_ $ (runTemplate ctx (entries files))


entries :: [FileInfo] -> Template (Html ())
entries files = do
  layout <- asks (.layout)
  let et = case layout of
             ListLayout      -> entry
             ThumbnailLayout -> thumbnail
  mconcat <$> traverse et files


entry :: FileInfo -> Template (Html ())
entry file = do
  root                <- asks (.root)
  selected            <- asks (.selected)
  TargetView target _ <- asks (.currentTarget)

  let ClientPathView { clientPath, hashPath } = asClientPathView root file.path

  pure do
    tr_ do
      td_ [ class_ "entry-preview " ] do
        fileNameElement file target True
          `with` Template.open root file
          `with`  [ class_ "field "]
      td_ $ modifiedDateElement file
      td_ $ sizeElement file
      `with`
        mconcat
          [ [ term "data-path" (Text.pack (coerce clientPath)) ]
          , [ class_ "selected confirmed " | clientPath `Selected.elem` selected]
          , [ id_ [i|tr-#{hashPath}|]
            , class_ "table-item "
            , draggable_ "true"
            ]
          , case file.content of
              Dir        -> [ class_ "dir "]
              Regular    -> mempty
          , case file.isLink of
              Link       -> [ class_ "symlink " ]
              BrokenLink -> [ class_ "broken-symlink "]
              NotLink    -> []
          ]


thumbnailLayout :: [FileInfo] -> Template (Html ())
thumbnailLayout files = do
  ctx <- ask
  pure do
    div_ [ id_ tableId, class_ "thumbnail-view " ] do
      tbody_ $ (runTemplate ctx (entries files))


thumbnail :: FileInfo -> Template (Html ())
thumbnail file = do
  TargetView target _ <- asks (.currentTarget)
  root                <- asks (.root)
  selected            <- asks (.selected)

  let ClientPathView { clientPath, hashPath } = asClientPathView root file.path

  pure do
    div_ do
      previewElement root file
      fileNameElement file target False `with` [ class_ "thumbnail-name" ]
      `with` mconcat
          [ [ term "data-path" (Text.pack (coerce clientPath)) ]
          , [ class_ "selected confirmed " | clientPath `Selected.elem` selected ]
          , [ id_ [i|tr-#{hashPath}|]
                         , class_ "thumbnail table-item "
                         , draggable_ "true"
            ]
          , case file.content of
              Dir        -> [ class_ "dir "]
              Regular    -> mempty
          , case file.isLink of
              Link       -> [ class_ "symlink " ]
              BrokenLink -> [ class_ "broken-symlink "]
              NotLink    -> []
          ]
        `with` Template.open root file


previewElement :: Root -> FileInfo -> Html ()
previewElement root file = do
  div_ [ class_ "thumbnail-preview " ] do
    div_ [  class_ "image-wrapper " ] do
      if
         | file.mimetype `isMime` "image" ->
           img_ [ loading_ "lazy"
                , src_ (linkToText (apiLinks.thumbnail (Just (ClientPath.toClientPath root file.path))))
                , draggable_ "false"
                ]
         | otherwise -> Template.icon file


fileNameElement :: FileInfo -> AnyTarget -> Bool -> Html ()
fileNameElement file target withIcon = do
  span_ do
    if withIcon
       then Template.icon file
       else mempty
    name
    `with` [ title_ (Text.pack displayName)
           ]
  where
    name = span_ (toHtml displayName)

    displayName =
      fromMaybe "-" $ handleTarget target
        [ targetHandler @S3      \_ -> coerce file.path
        , targetHandler @FileSys \_ -> coerce takeFileName file.path
        , targetHandler @DummyTarget \_ -> coerce takeFileName file.path
        ]


sizeElement :: FileInfo -> Html ()
sizeElement file =
  span_ (toHtml displaySize)
    `with` [ class_ "field file-meta "
           , title_ (Text.pack displaySize)
           ]
  where
    displaySize = toReadableSize (fromMaybe 0 file.size)


modifiedDateElement :: FileInfo -> Html ()
modifiedDateElement file =
  span_ (toHtml displayTime)
    `with` [ class_ "field file-meta "
           , title_ (Text.pack displayTime)
           ]
  where
    displayTime = maybe mempty (formatTime defaultTimeLocale "%Y/%m/%d") file.mtime


sortControl :: SortFileBy -> [Attribute]
sortControl o =
    [ hxGet (apiLinks.sortTable (Just o))
    , hxSwap OuterHTML
    , hxTarget "#index"
    ]


------------------------------------
-- context menu
------------------------------------


contextMenu1 :: FileInfo -> Template (Html ())
contextMenu1 file = do
  root     <- asks (.root)
  readOnly <- asks (.readOnly)
  Phrase
    { contextmenu_delete
    , contextmenu_details
    , contextmenu_open
    , contextmenu_view
    , contextmenu_play
    , contextmenu_copy
    , contextmenu_edit
    , contextmenu_rename
    , contextmenu_download
    , confirm_delete1
    } <- phrase <$> asks (.locale)
  pure do
    let clientPath@(ClientPath cp)  = ClientPath.toClientPath root file.path
    let textClientPath = Text.pack cp

    div_ [ class_ "dropdown-content " , id_ contextMenuId ] do
      let dropDownItem = div_ [ class_ "dropdown-item" ]
      case file.content of
        Regular
          | file.mimetype `isMime` "application/pdf" -> dropDownItem do i_ [ class_ "bx bx-show" ] mempty >> span_ (toHtml contextmenu_view)
          | file.mimetype `isMime` "audio"           -> dropDownItem do i_ [ class_ "bx bx-play" ] mempty >> span_ (toHtml contextmenu_play)
          | file.mimetype `isMime` "video"           -> dropDownItem do i_ [ class_ "bx bx-play" ] mempty >> span_ (toHtml contextmenu_play)
          | file.mimetype `isMime` "image"           -> dropDownItem do i_ [ class_ "bx bx-show" ] mempty >> span_ (toHtml contextmenu_view)
          | file.mimetype `isMime` "text"            -> dropDownItem do i_ [ class_ "bx bxs-edit" ] mempty >> span_ (toHtml contextmenu_edit)
          | otherwise -> mempty
        Dir        -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bxs-folder-open" ] mempty >> span_ (toHtml contextmenu_open)
        `with` Template.open root file

      div_ [ class_ "dropdown-item"
           , hxGet (apiLinks.copy1 (Just clientPath))
           , hxTarget "#index"
           , hxSwap OuterHTML
           ] do
        i_ [ class_ "bx bx-detail" ] mempty
        span_ (toHtml contextmenu_copy)

      a_ [ class_ "dropdown-item" ,  href_ (linkToText (apiLinks.download [clientPath])) ] do
        i_ [ class_ "bx bx-download" ] mempty
        span_ (toHtml contextmenu_download)

      case readOnly of
        True -> mempty
        False -> do
          div_ [ class_ "dropdown-item"
               , hxGet (apiLinks.renameModal (Just clientPath))
               , hxTarget "#index"
               , hxSwap BeforeEnd
               ] do
            i_ [ class_ "bx bxs-rename" ] mempty
            span_ (toHtml contextmenu_rename)

          div_ [ class_ "dropdown-item"
               , hxDelete (apiLinks.delete [clientPath] False)
               , hxSwap None
               , hxConfirm (Text.replace "{}" textClientPath confirm_delete1)
               ] do
            i_ [ class_ "bx bxs-trash" ] mempty
            span_ (toHtml contextmenu_delete)

      div_ [ class_ "dropdown-item"
           , hxGet (apiLinks.fileDetailModal (Just clientPath))
           , hxTarget "#index"
           , hxSwap BeforeEnd
           ] do
        i_ [ class_ "bx bx-detail" ] mempty
        span_ (toHtml contextmenu_details)


contextMenuMany :: [ClientPath] -> Template (Html ())
contextMenuMany clientPaths = do
  readOnly <- asks (.readOnly)
  Phrase
    { contextmenu_delete_local
    , contextmenu_selected
    , contextmenu_copy
    , contextmenu_cancel
    , contextmenu_download
    , confirm_delete_local
    } <- phrase <$> asks (.locale)

  pure do
    div_ [ class_ "dropdown-content " , id_ contextMenuId ] do
      div_ [ class_ "dropdown-item no-effect" ] do
        i_ [ class_ "bx bx-select-multiple" ] mempty
        span_ [i|#{length clientPaths} #{contextmenu_selected}|]
      hr_ []
      case readOnly of
        True -> mempty
        False -> do
          div_ [ class_ "dropdown-item"
               , hxDelete (apiLinks.delete clientPaths False)
               , hxSwap None
               , hxConfirm (Text.replace "{}" (Text.pack (show (length clientPaths))) confirm_delete_local)
               ] do
            i_ [ class_ "bx bxs-trash" ] mempty
            span_ (toHtml contextmenu_delete_local)

          div_ [ class_ "dropdown-item"
               , hxGet apiLinks.copy
               , hxTarget "#control-panel"
               , hxSwap OuterHTML
               ] do
            i_ [ class_ "bx bx-detail" ] mempty
            span_ (toHtml contextmenu_copy)

      a_ [ class_ "dropdown-item" ,  href_ (linkToText (apiLinks.download clientPaths)) ] do
        i_ [ class_ "bx bx-download" ] mempty
        span_ (toHtml contextmenu_download)

      div_ [ class_ "dropdown-item"
           , hxPost apiLinks.cancel
           , hxTarget "#index"
           , hxSwap OuterHTML
           ] do
        i_ [ class_ "bx bx-message-alt-x" ] mempty
        span_ (toHtml contextmenu_cancel)


sortBtn :: Template (Html ())
sortBtn = do
  order <- asks (.sortedBy)
  Phrase
    { detail_filename
    , detail_modified
    , detail_size
    } <- phrase <$> asks (.locale)
  pure do
    div_ [ class_ "control-panel-dropdown-btn" ] do
      button_ [ class_ "btn btn-control " ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bx-sort-alt-2" ] mempty
      div_ [ class_ "dropdown-content " ] do
        let item :: Html () -> Html () -> Html ()
            item label ico = div_ [ class_ "dropdown-item" ] do
              ico
              span_ label
        item (toHtml detail_filename) (sortIconName order)  `with` sortControlName order
        item (toHtml detail_modified) (sortIconMTime order) `with` sortControlMTime order
        item (toHtml detail_size)     (sortIconSize order)  `with` sortControlSize order


sortIconName :: SortFileBy -> Html ()
sortIconName = \case
  ByNameUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
  ByNameDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
  _          -> i_ [ class_ "bx bx-sort"] mempty


sortIconMTime :: SortFileBy -> Html ()
sortIconMTime = \case
  ByModifiedUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
  ByModifiedDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
  _              -> i_ [ class_ "bx bx-sort"] mempty


sortIconSize :: SortFileBy -> Html ()
sortIconSize = \case
  BySizeUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
  BySizeDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
  _          -> i_ [ class_ "bx bx-sort"] mempty


sortControlName :: SortFileBy -> [Attribute]
sortControlName = \case
  ByNameUp   -> sortControl ByNameDown
  ByNameDown -> sortControl ByNameUp
  _          -> sortControl ByNameUp


sortControlMTime :: SortFileBy -> [Attribute]
sortControlMTime = \case
  ByModifiedUp   -> sortControl ByModifiedDown
  ByModifiedDown -> sortControl ByModifiedUp
  _              -> sortControl ByModifiedUp


sortControlSize :: SortFileBy -> [Attribute]
sortControlSize = \case
  BySizeUp   -> sortControl BySizeDown
  BySizeDown -> sortControl BySizeUp
  _          -> sortControl BySizeUp


------------------------------------
-- component ids
------------------------------------


newFileModalId :: Text
newFileModalId = "new-file-modal"

newFolderModalId :: Text
newFolderModalId = "new-folder-modal"

fileDetailModalId :: Text
fileDetailModalId = "file-detail-modal"

renameModalId :: Text
renameModalId = "rename-modal"

editorModalId :: Text
editorModalId = "editor-modal"

contextMenuId :: Text
contextMenuId = "contextmenu"
