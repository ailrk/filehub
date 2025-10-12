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
  , newFileModal
  , newFolderModal
  , fileDetailModal
  , editorModal
  , contextMenu1
  , contextMenuMany
  , table
  , themeBtn
  , localeBtn
  )
  where

import Control.Monad (when, join)
import Data.ByteString (ByteString)
import Data.ClientPath (ClientPath(..))
import Data.ClientPath qualified as ClientPath
import Data.File (File(..), FileType(..), FileInfo)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (iii, i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Format (formatTime, defaultTimeLocale)
import Effectful.Reader.Dynamic (asks)
import Filehub.Links ( apiLinks, linkToText )
import Filehub.Locale (Locale(..), Phrase (..), phrase)
import Filehub.Routes (Api(..))
import Filehub.Selected qualified as Selected
import Filehub.Session (TargetView(..))
import Filehub.Size (toReadableSize)
import Filehub.Template (Template, TemplateContext(..))
import Filehub.Template.Shared (bold, sideBarId, viewId, searchBar, tableId)
import Filehub.Template.Shared qualified as Template
import Filehub.Theme (Theme (..))
import Filehub.Types (Layout(..), SortFileBy(..))
import Lens.Micro.Platform ()
import Lucid
import Network.Mime.Extended (isMime)
import System.FilePath (takeFileName)
import Target.Dummy (DummyTarget)
import Target.File (FileSys, TargetBackend (..))
import Target.S3 (S3, TargetBackend (..))
import Target.Types (targetHandler, Target, handleTarget)
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
  pure do
    div_ [ id_ "index" ] do
      sideBar'
      controlPanel'
      toolBar'
      view'


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


sideBar :: [(Target, Int)] -> TargetView -> Template (Html ())
sideBar targets (TargetView currentTarget _) = do
  p <- phrase <$> asks @TemplateContext (.locale)
  pure do
    div_ [ id_ sideBarId ] do
      traverse_ (targetTab p) targets
  where
    targetTab :: Phrase -> (Target, Int) -> Html ()
    targetTab Phrase { target_filesystem, target_s3 } (target, selectedCount) = do
      div_ [ class_ "target-tab"
           , term "hx-get" (linkToText (apiLinks.changeTarget (Just (Target.getTargetId target))))
           , term "hx-target" "#index"
           , term "hx-swap" "outerHTML"
           ] do
        span_ [ class_ "field "] do
          fromMaybe "unknown" $ handleTarget target
            [ targetHandler @S3      \_ -> i_ [ class_ "bx bxs-cube" ] mempty
            , targetHandler @FileSys \_ -> i_ [ class_ "bx bx-folder" ] mempty
            ]

          fromMaybe "" $ handleTarget target
            [ targetHandler @S3      \(S3Backend { bucket }) -> span_ [iii| /#{bucket} |]
            , targetHandler @FileSys \(FileBackend { root }) -> span_ [iii| /#{takeFileName root} |]
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
            , targetHandler @FileSys \(FileBackend { root }) ->
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
    <*> (Just <$> layoutBtn)
    <*> pure Nothing


newFolderBtn :: Template (Html ())
newFolderBtn = do
  Phrase { control_panel_new_folder } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    button_ [ class_ "btn btn-control "
            , type_ "submit"
            , term "hx-get" (linkToText apiLinks.newFolderModal)
            , term "hx-target" "#index"
            , term "hx-swap" "beforeend"
            , term "data-btn-title" control_panel_new_folder
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-folder-plus" ] mempty


newFileBtn :: Template (Html ())
newFileBtn = do
  Phrase { control_panel_new_file } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    button_ [ class_ "btn btn-control"
            , type_ "submit"
            , term "hx-get" (linkToText apiLinks.newFileModal)
            , term "hx-target" "#index"
            , term "hx-swap" "beforeend"
            , term "data-btn-title" control_panel_new_file
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-file-plus" ] mempty


uploadBtn :: Template (Html ())
uploadBtn = do
  Phrase { control_panel_upload } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    let fileInputId = "file-input"
    input_ [ type_ "file"
           , name_ "file"
           , id_ fileInputId
           , style_ "display:none"
           , multiple_ ""
           , term "hx-encoding" "multipart/form-data"
           , term "hx-post" (linkToText apiLinks.upload)
           , term "hx-target" "#index"
           , term "hx-swap" "outerHTML"
           , term "hx-trigger" "change"
           ]

    button_ [ class_ "btn btn-control"
            , onclick_ [iii|document.querySelector('\##{fileInputId}').click()|]
            , term "data-btn-title" control_panel_upload
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-upload" ] mempty


copyBtn :: Template (Html ())
copyBtn = do
  Phrase { control_panel_copy } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    button_ [ class_ "btn btn-control"
            , type_ "submit"
            , term "hx-get" (linkToText apiLinks.copy)
            , term "hx-target" "#control-panel"
            , term "hx-swap" "outerHTML"
            , term "data-btn-title" control_panel_copy
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-copy-alt" ] mempty


pasteBtn :: Template (Html ())
pasteBtn = do
  Phrase { control_panel_paste } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    button_ [ class_ "btn btn-control"
            , type_ "submit"
            , term "hx-post" (linkToText apiLinks.paste)
            , term "hx-target" "#index"
            , term "hx-swap" "outerHTML"
            , term "data-btn-title" control_panel_paste
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-paste" ] mempty


deleteBtn :: Template (Html ())
deleteBtn = do
  selected <- asks @TemplateContext (.selected)
  Phrase
    { control_panel_delete
    , confirm_delete_all
    } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    button_ [ class_ "btn btn-control urgent"
            , type_ "submit"
            , term "hx-delete" (linkToText (apiLinks.delete (Selected.toList selected) True))
            , term "hx-target" "#index"
            , term "hx-swap" "outerHTML"
            , term "hx-confirm" confirm_delete_all
            , term "data-btn-title" control_panel_delete
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-trash" ] mempty


cancelBtn :: Template (Html ())
cancelBtn = do
  Phrase { control_panel_cancel } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    button_ [ class_ "btn btn-control"
            , type_ "submit"
            , term "hx-post" (linkToText apiLinks.cancel)
            , term "hx-target" "#index"
            , term "hx-swap" "outerHTML"
            , term "data-btn-title" control_panel_cancel
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-message-alt-x" ] mempty


logoutBtn :: Template (Html ())
logoutBtn = do
  Phrase { confirm_logout } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    button_ [ class_ "btn btn-control urgent "
            , type_ "submit"
            , term "hx-post" (linkToText apiLinks.logout)
            , term "hx-target" "#index"
            , term "hx-swap" "outerHTML"
            , term "hx-confirm" confirm_logout
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-power-off" ] mempty


themeBtn :: Template (Html ())
themeBtn = do
  Phrase { control_panel_dark, control_panel_light } <- phrase <$> asks @TemplateContext (.locale)
  theme <- asks @TemplateContext (.theme)
  pure do
    case theme of
      Light -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , term "hx-get" (linkToText apiLinks.toggleTheme)
                , term "hx-target" "#index"
                , term "hx-swap" "outerHTML"
                , term "data-btn-title" control_panel_dark
                ] do
          i_ [ class_ "bx bxs-moon" ] mempty
      Dark -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , term "hx-get" (linkToText apiLinks.toggleTheme)
                , term "hx-target" "#index"
                , term "hx-swap" "outerHTML"
                , term "data-btn-title" control_panel_light
                ] do
          i_ [ class_ "bx bxs-sun" ] mempty


layoutBtn :: Template (Html ())
layoutBtn =  do
  layout <- asks @TemplateContext (.layout)
  Phrase { control_panel_grid, control_panel_list } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    case layout of
      ListLayout -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , term "hx-get" (linkToText (apiLinks.selectLayout (Just ThumbnailLayout)))
                , term "hx-target" "#index"
                , term "hx-swap" "outerHTML"
                , term "data-btn-title" control_panel_grid
                ] do
          i_ [ class_ "bx bxs-grid-alt" ] mempty
      ThumbnailLayout -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , term "hx-get" (linkToText (apiLinks.selectLayout (Just ListLayout)))
                , term "hx-target" "#index"
                , term "hx-swap" "outerHTML"
                , term "data-btn-title" control_panel_list
                ] do
          i_ [ class_ "bx bx-menu" ] mempty


localeBtn :: Html ()
localeBtn =
  div_ [ id_ "locale" ] do
    button_ [ class_ "btn btn-control "
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-world" ] mempty
    div_ [ class_ "dropdown-content " ] do
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just EN)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "English"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just ZH_CN)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "简体中文"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just ZH_TW)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "繁體中文"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just ZH_HK)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "繁體中文"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just JA)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "日本語"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just ES)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "Español"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just FR)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "Français"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just DE)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "Deutsch"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just KO)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "한국어"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just RU)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "Русский"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just PT)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "Português"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.changeLocale (Just IT)), term "hx-target" "#index", term "hx-swap" "outerHTML" ] do span_ "Italiano"


------------------------------------
-- modals
------------------------------------


newFileModal :: Template (Html ())
newFileModal = do
  Phrase
    { modal_file
    , modal_create
    , placeholder_newfile
    } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    modal [ id_ newFileModalId ] do
      span_ [ class_ "modal-title-bar " ] do
        bold (toHtml modal_file)
        div_ [ class_ "title-bar-btn btn-modal-close "
             , term "_" "on click trigger Close"
             ] do
          i_ [ class_ "bx bx-x"] mempty
      br_ mempty
      form_ [ term "hx-post" (linkToText apiLinks.newFile)
            , term "hx-target" "#view"
            , term "hx-swap" "outerHTML"
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
    } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    modal [ id_ newFolderModalId ] do
      span_ [ class_ "modal-title-bar " ] do
        bold (toHtml modal_folder)
        div_ [ class_ "title-bar-btn btn-modal-close "
             , term "_" "on click trigger Close"
             ] do
          i_ [ class_ "bx bx-x"] mempty
      br_ mempty
      form_ [ term "hx-post" (linkToText (apiLinks.newFolder))
            , term "hx-target" "#view"
            , term "hx-swap" "outerHTML"
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


fileDetailModal :: FileInfo -> Template (Html ())
fileDetailModal file = do
  Phrase
    { modal_detail
    , detail_filename
    , detail_modified
    , detail_accessed
    , detail_size
    , detail_content_type
    } <- phrase <$> asks @TemplateContext (.locale)

  pure do
    modal [ id_ fileDetailModalId ] do
      bold (toHtml modal_detail)
      br_ mempty
      table_ do
        tbody_ do
          tr_ do
            td_ (toHtml detail_filename)
            td_ (toHtml (takeFileName file.path))
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


editorModal :: FilePath -> ByteString -> Template (Html ())
editorModal filename content = do
  readOnly <- asks @TemplateContext (.readOnly)
  Phrase
    { modal_edit
    , modal_readonly
    , placeholder_empty_file
    , placeholder_filename
    , confirm_save_edit
    } <- phrase <$> asks @TemplateContext (.locale)

  pure do
    modal [ id_ editorModalId ] do

      case readOnly of
        True -> bold (toHtml modal_readonly)
        False ->  do
          span_ [ class_ "modal-title-bar " ] do
            bold  (toHtml modal_edit)
            div_ [ class_ "title-bar-btn btn-modal-close "
                 , term "_" "on click trigger Close"
                 ] do
              i_ [ class_ "bx bx-x"] mempty

      br_ mempty

      form_ [ term "hx-post" (linkToText (apiLinks.updateFile))
            , term "hx-confirm" (Text.replace "{}" (Text.pack filename) confirm_save_edit)
            ] do
        input_ [ class_ "form-control "
               , type_ "text"
               , name_ "path"
               , value_ (Text.pack filename)
               , placeholder_ placeholder_filename
               ]

        br_ mempty
        br_ mempty

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
  layout <- asks @TemplateContext (.layout)
  case layout of
    ListLayout      -> listLayout files
    ThumbnailLayout -> thumbnailLayout files


listLayout :: [FileInfo]  -> Template (Html ())
listLayout files = do
  root <- asks @TemplateContext (.root)
  selected <- asks @TemplateContext (.selected)
  order <- asks @TemplateContext (.sortedBy)
  TargetView target _ <- asks @TemplateContext (.currentTarget)
  Phrase
    { detail_filename
    , detail_modified
    , detail_size
    } <- phrase <$> asks @TemplateContext (.locale)

  let record :: (Int, FileInfo) -> Html ()
      record (idx, file) = do
        let clientPath@(ClientPath path) = ClientPath.toClientPath root file.path
        tr_ do
          td_ $ fileNameElement file target True
                  `with` Template.open root file
                  `with`  [ class_ "field "]
          td_ $ modifiedDateElement file
          td_ $ sizeElement file
          `with`
            mconcat
              [ [ term "data-path" (Text.pack path) ]
              , [ class_ "selected confirmed " | clientPath `Selected.elem` selected]
              , [ id_ [i|tr-#{idx}|]
                , class_ "table-item "
                , draggable_ "true"
                ]
              , case file.content of
                  Dir     -> [ class_ "dir "]
                  Regular -> mempty
              ]
  let sortIconName =
        case order of
          ByNameUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
          ByNameDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
          _          -> i_ [ class_ "bx bx-sort"] mempty
  let sortIconMTime =
        case order of
          ByModifiedUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
          ByModifiedDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
          _              -> i_ [ class_ "bx bx-sort"] mempty
  let sortIconSize =
        case order of
          BySizeUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
          BySizeDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
          _          -> i_ [ class_ "bx bx-sort"] mempty
  let sortControlName =
        case order of
          ByNameUp   -> sortControl ByNameDown
          ByNameDown -> sortControl ByNameUp
          _          -> sortControl ByNameUp
  let sortControlMTime =
        case order of
          ByModifiedUp   -> sortControl ByModifiedDown
          ByModifiedDown -> sortControl ByModifiedUp
          _              -> sortControl ByModifiedUp
  let sortControlSize =
        case order of
          BySizeUp   -> sortControl BySizeDown
          BySizeDown -> sortControl BySizeUp
          _          -> sortControl BySizeUp
  pure do
    table_ [ id_ tableId, class_ "list-view " ] do
      thead_ do
        tr_ do
          th_ do
            span_ [ class_ "field " ] do
              (toHtml detail_filename)
              sortIconName
            `with` sortControlName
          th_ do
            span_ [ class_ "field " ] do
              (toHtml detail_modified)
              sortIconMTime
              `with` sortControlMTime
          th_ do
            span_ [ class_ "field " ] do
              (toHtml detail_size)
              sortIconSize
              `with` sortControlSize
      tbody_ $ traverse_ record ([0..] `zip` files)


thumbnailLayout :: [FileInfo] -> Template (Html ())
thumbnailLayout files = do
  root <- asks @TemplateContext (.root)
  selected <- asks @TemplateContext (.selected)
  TargetView target _ <- asks @TemplateContext (.currentTarget)
  let thumbnail :: (Int, FileInfo) -> Html ()
      thumbnail (idx, file) = card `with` Template.open root file
        where
          card = div_ do
            previewElement root file
            fileNameElement file target False `with` [ class_ "thumbnail-name" ]
            `with`
              mconcat
                [ [ term "data-path" (Text.pack path) ]
                , [ class_ "selected confirmed " | clientPath `Selected.elem` selected ]
                , [ id_ [i|tr-#{idx}|]
                  , class_ "thumbnail table-item "
                  , draggable_ "true"
                  ]
                , case file.content of
                    Dir     -> [ class_ "dir "]
                    Regular -> mempty
                ]
          clientPath@(ClientPath path) = ClientPath.toClientPath root file.path

  pure do
    div_ [ id_ tableId, class_ "thumbnail-view " ] do
      tbody_ $ traverse_ thumbnail ([0..] `zip` files)


previewElement :: FilePath -> FileInfo -> Html ()
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


fileNameElement :: FileInfo -> Target -> Bool -> Html ()
fileNameElement file target withIcon = do
  span_ ((if withIcon then Template.icon file else mempty) >> name)
    `with` [ title_ (Text.pack displayName) ]
  where
    name = span_ (toHtml displayName)

    displayName =
      fromMaybe "-" $ handleTarget target
        [ targetHandler @S3      \_ -> file.path
        , targetHandler @FileSys \_ -> takeFileName file.path
        , targetHandler @DummyTarget \_ -> takeFileName file.path
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
    [ term "hx-get" (linkToText (apiLinks.sortTable (Just o)))
    , term "hx-swap" "outerHTML"
    , term "hx-target" "#view"
    ]


------------------------------------
-- context menu
------------------------------------


contextMenu1 :: FileInfo -> Template (Html ())
contextMenu1 file = do
  root <- asks @TemplateContext (.root)
  readOnly <- asks @TemplateContext (.readOnly)
  Phrase
    { contextmenu_delete
    , contextmenu_details
    , contextmenu_open
    , contextmenu_view
    , contextmenu_play
    , contextmenu_copy
    , contextmenu_edit
    , contextmenu_download
    , confirm_delete1
    } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    let clientPath@(ClientPath cp)  = ClientPath.toClientPath root file.path
    let textClientPath = Text.pack cp

    div_ [ class_ "dropdown-content " , id_ contextMenuId ] do
      case file.content of
        Dir -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bxs-folder-open" ] mempty >> span_ (toHtml contextmenu_open)
        Regular
          | file.mimetype `isMime` "application/pdf" -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bx-show" ] mempty >> span_ (toHtml contextmenu_view)
          | file.mimetype `isMime` "audio"           -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bx-play" ] mempty >> span_ (toHtml contextmenu_play)
          | file.mimetype `isMime` "video"           -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bx-play" ] mempty >> span_ (toHtml contextmenu_play)
          | file.mimetype `isMime` "image"           -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bx-show" ] mempty >> span_ (toHtml contextmenu_view)
          | file.mimetype `isMime` "text"            -> div_ [ class_ "dropdown-item" ] do i_ [ class_ "bx bxs-edit" ] mempty >> span_ (toHtml contextmenu_edit)
          | otherwise -> mempty
        `with` Template.open root file

      div_ [ class_ "dropdown-item"
           , term "hx-get" (linkToText (apiLinks.copy1 (Just clientPath)))
           , term "hx-target" "#index"
           , term "hx-swap" "outerHTML"
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
               , term "hx-delete" (linkToText (apiLinks.delete [clientPath] False))
               , term "hx-target" "#index"
               , term "hx-swap" "outerHTML"
               , term "hx-confirm" (Text.replace "{}" textClientPath confirm_delete1)
               ] do
            i_ [ class_ "bx bxs-trash" ] mempty
            span_ (toHtml contextmenu_delete)

      div_ [ class_ "dropdown-item"
           , term "hx-get" (linkToText (apiLinks.fileDetailModal (Just clientPath)))
           , term "hx-target" "#index"
           , term "hx-swap" "beforeend"
           ] do
        i_ [ class_ "bx bx-detail" ] mempty
        span_ (toHtml contextmenu_details)


contextMenuMany :: [ClientPath] -> Template (Html ())
contextMenuMany clientPaths = do
  readOnly <- asks @TemplateContext (.readOnly)
  Phrase
    { contextmenu_delete_local
    , contextmenu_selected
    , contextmenu_copy
    , contextmenu_cancel
    , contextmenu_download
    , confirm_delete_local
    } <- phrase <$> asks @TemplateContext (.locale)

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
               , term "hx-delete" (linkToText (apiLinks.delete clientPaths False))
               , term "hx-target" "#index"
               , term "hx-swap" "outerHTML"
               , term "hx-confirm" (Text.replace "{}" (Text.pack (show (length clientPaths))) confirm_delete_local)
               ] do
            i_ [ class_ "bx bxs-trash" ] mempty
            span_ (toHtml contextmenu_delete_local)

          div_ [ class_ "dropdown-item"
               , term "hx-get" (linkToText apiLinks.copy)
               , term "hx-target" "#control-panel"
               , term "hx-swap" "outerHTML"
               ] do
            i_ [ class_ "bx bx-detail" ] mempty
            span_ (toHtml contextmenu_copy)

      a_ [ class_ "dropdown-item" ,  href_ (linkToText (apiLinks.download clientPaths)) ] do
        i_ [ class_ "bx bx-download" ] mempty
        span_ (toHtml contextmenu_download)

      div_ [ class_ "dropdown-item"
           , term "hx-post" (linkToText apiLinks.cancel)
           , term "hx-target" "#index"
           , term "hx-swap" "outerHTML"
           ] do
        i_ [ class_ "bx bx-message-alt-x" ] mempty
        span_ (toHtml contextmenu_cancel)



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
