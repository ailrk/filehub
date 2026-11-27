{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Filehub.Template.Desktop
  ( index
  , controlPanel
  , view
  , toolBar
  , entry
  , entries
  , thumbnail
  , editorModal
  , table
  )
  where

import Filehub.Prelude
import Control.Monad.Reader (asks, MonadReader (..))
import Data.ClientPath (ClientPath(..), AbsPath (..))
import Data.ClientPath.View (ClientPathView(..), AsClientPathView (..))
import Data.File (File(..), FileType(..), FileInfo, IsLink (..))
import Data.String.Interpolate (iii)
import Filehub.Locale (Locale(..), Phrase (..), phrase)
import Filehub.Routes (Api(..))
import Filehub.Routes.Links ( apiLinks )
import Filehub.Session (TargetView(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (Layout(..), Selected(..))
import Filehub.Template (Template, TemplateContext(..), runTemplate)
import Filehub.Template.Htmx (asHtmx)
import Filehub.Template.Common
  ( bold, modal, viewId, searchBar, tableId
  , fileNameElement, sizeElement, modifiedDateElement
  , sortIconName, sortIconMTime, sortIconSize, sortByText
  )
import Filehub.Template.Common qualified as Template
import Filehub.Theme (Theme (..))
import Lucid
import Lucid.Htmx (hxGet, hxEncoding, hxPost, hxTrigger, Trigger (..), hxDelete, hxConfirm, HtmxEvent (..), hxOn)
import Network.Mime.Extended (isMime)
import Servant.Extended (linkToText)
import System.FilePath (takeFileName)


------------------------------------
-- helpers
------------------------------------


fileType :: FileInfo -> Text
fileType file = case file.content of
  Dir     -> "dir"
  Regular
    | file.mimetype `isMime` "image"           -> "image"
    | file.mimetype `isMime` "video"           -> "video"
    | file.mimetype `isMime` "audio"           -> "audio"
    | file.mimetype `isMime` "application/pdf" -> "pdf"
    | file.mimetype `isMime` "text"            -> "text"
    | otherwise                                -> "file"



------------------------------------
-- components
------------------------------------


index :: Html ()
      -> Html ()
      -> Html ()
      -> Html ()
      -> Template (Html ())
index i18nScript sideBar' view' toolBar' = do
  controlPanel' <- controlPanel
  sidebarCollapsed <- asks (.sidebarCollapsed)
  pure do
    div_ [ id_ "index" ] do
      i18nScript
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


controlPanel :: Template (Html ())
controlPanel = join do
  Template.controlPanel
    <$> pure localeBtn
    <*> pure mempty
    <*> pure mempty
    <*> uploadBtn
    <*> copyBtn
    <*> cutBtn
    <*> pasteBtn
    <*> deleteBtn
    <*> cancelBtn
    <*> themeBtn
    <*> logoutBtn
    <*> (Just <$> sortBtn)
    <*> (Just <$> toggleSidebarBtn)
    <*> (Just <$> layoutBtn)
    <*> pure Nothing
    <*> (Just <$> shareLinksBtn)
    <*> (Just <$> historyBtn)
    <*> pure (Just settingsBtn)
    <*> pure Nothing


uploadBtn :: Template (Html ())
uploadBtn = do
  Phrase { control_panel_upload } <- phrase <$> asks (.locale)
  pure do
    let fileInputId = "file-input"
    input_ (asHtmx @"upload" hxPost ()
           [ type_ "file"
           , name_ "file"
           , id_ fileInputId
           , style_ "display:none"
           , multiple_ ""
           , hxEncoding "multipart/form-data"
           , hxTrigger Change
           ])

    button_ [ class_ "btn btn-control tooltip"
            , onclick_ [iii|document.querySelector('\##{fileInputId}').click()|]
            , term "data-tooltip" control_panel_upload
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-upload" ] mempty


copyBtn :: Template (Html ())
copyBtn = do
  Phrase { control_panel_copy } <- phrase <$> asks (.locale)
  selected <- asks (.selected)
  let clientPaths = case selected of
        NoSelection   -> []
        Selected x xs -> x:xs
  pure do
    button_ (asHtmx @"copy" hxGet clientPaths
            [ class_ "btn btn-control tooltip"
            , type_ "submit"
            , term "data-cancel-hide" ""
            , term "data-tooltip" control_panel_copy
            ]) do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-copy-alt" ] mempty


cutBtn :: Template (Html ())
cutBtn = do
  Phrase { control_panel_cut } <- phrase <$> asks (.locale)
  selected <- asks (.selected)
  let clientPaths = case selected of
        NoSelection   -> []
        Selected x xs -> x:xs
  pure do
    button_ (asHtmx @"cut" hxGet clientPaths
            [ class_ "btn btn-control tooltip"
            , type_ "submit"
            , term "data-cancel-hide" ""
            , term "data-tooltip" control_panel_cut
            ]) do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-cut" ] mempty


pasteBtn :: Template (Html ())
pasteBtn = do
  Phrase { control_panel_paste } <- phrase <$> asks (.locale)
  pure do
    button_ (asHtmx @"paste" hxPost ()
            [ class_ "btn btn-control tooltip"
            , type_ "submit"
            , term "data-cancel-hide" ""
            , term "data-tooltip" control_panel_paste
            ]) do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-paste" ] mempty


deleteBtn :: Template (Html ())
deleteBtn = do
  Phrase
    { control_panel_delete
    , confirm_delete_all
    } <- phrase <$> asks (.locale)
  pure do
    button_ (asHtmx @"delete" hxDelete ([], True)
            [ class_ "btn btn-control urgent tooltip"
            , type_ "submit"
            , term "data-cancel-hide" ""
            , hxConfirm confirm_delete_all
            , term "data-tooltip" control_panel_delete
            ]) do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-trash" ] mempty


cancelBtn :: Template (Html ())
cancelBtn = do
  Phrase { control_panel_cancel } <- phrase <$> asks (.locale)
  pure do
    button_ (asHtmx @"cancel" hxPost ()
            [ class_ "btn btn-control tooltip"
            , type_ "submit"
            , term "data-cancel-hide" ""
            , term "data-tooltip" control_panel_cancel
            ]) do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bxs-message-alt-x" ] mempty


toggleSidebarBtn :: Template (Html ())
toggleSidebarBtn = do
  Phrase { toggle_sidebar } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control tooltip"
            , type_ "button"
            , term "data-action" "toggle-sidebar"
            , term "data-tooltip" toggle_sidebar
            ] do
      i_ [ class_ "bx bx-sidebar" ] mempty


logoutBtn :: Template (Html ())
logoutBtn = do
  Phrase { confirm_logout } <- phrase <$> asks (.locale)
  pure do
    button_ (asHtmx @"logout" hxPost ()
            [ class_ "btn btn-control urgent "
            , type_ "submit"
            , hxConfirm confirm_logout
            ]) do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-power-off" ] mempty


themeBtn :: Template (Html ())
themeBtn = do
  Phrase { control_panel_dark, control_panel_light } <- phrase <$> asks (.locale)
  theme <- asks (.theme)
  pure do
    let (tooltip, iconClass, themeName) = case theme of
          Light -> (control_panel_dark,  "bx bxs-moon", "light" :: Text)
          Dark  -> (control_panel_light, "bx bxs-sun",  "dark"  :: Text)
    button_ [ class_ "btn btn-control tooltip"
            , type_ "button"
            , term "data-action" "toggle-theme"
            , term "data-current-theme" themeName
            , term "data-tooltip" tooltip
            ] do
      i_ [ class_ iconClass ] mempty


layoutBtn :: Template (Html ())
layoutBtn =  do
  layout <- asks (.layout)
  Phrase { control_panel_grid, control_panel_list } <- phrase <$> asks (.locale)
  pure do
    case layout of
      ListLayout -> do
        button_ (asHtmx @"selectLayout" hxGet (Just ThumbnailLayout)
                [ class_ "btn btn-control tooltip"
                , type_ "submit"
                , term "data-tooltip" control_panel_grid
                ]) do
          i_ [ class_ "bx bxs-grid-alt" ] mempty
      ThumbnailLayout -> do
        button_ (asHtmx @"selectLayout" hxGet (Just ListLayout)
                [ class_ "btn btn-control tooltip"
                , type_ "submit"
                , term "data-tooltip" control_panel_list
                ]) do
          i_ [ class_ "bx bx-menu" ] mempty


shareLinksBtn :: Template (Html ())
shareLinksBtn = do
  Phrase { control_panel_share_links } <- phrase <$> asks (.locale)
  pure do
    button_ [ class_ "btn btn-control tooltip"
            , type_ "button"
            , term "hx-get" "/share/links"
            , term "hx-target" "#index"
            , term "hx-swap" "beforeend"
            , term "data-tooltip" control_panel_share_links
            ] do
      i_ [ class_ "bx bxs-share-alt" ] mempty


historyBtn :: Template (Html ())
historyBtn = pure do
  button_ [ class_ "btn btn-control tooltip"
          , type_ "button"
          , term "hx-get" "/history"
          , term "hx-target" "#index"
          , term "hx-swap" "beforeend"
          , term "data-tooltip" "History"
          ] do
    i_ [ class_ "bx bx-history" ] mempty


settingsBtn :: Html ()
settingsBtn =
  button_ [ class_ "btn btn-control tooltip"
          , type_ "button"
          , term "hx-get" (linkToText apiLinks.settingsPanel)
          , term "hx-target" "body"
          , term "hx-swap" "beforeend"
          , term "data-tooltip" "Settings"
          ] do
    i_ [ class_ "bx bx-cog" ] mempty


localeBtn :: Html ()
localeBtn =
  div_ [ class_ "control-panel-dropdown-btn" ] do
    button_ [ class_ "btn btn-control " ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-world" ] mempty
    div_ [ class_ "dropdown-content " ] do
      let item :: Locale -> Html () -> Html ()
          item loc label = div_ (asHtmx @"changeLocale" hxGet (Just loc)
                                [ class_ "dropdown-item" ]) do span_ label
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
      form_ (asHtmx @"updateFile" hxPost ()
            [ hxConfirm (txt_replace "{}" (txt_pack filename) confirm_save_edit)
            , hxOn AfterRequest "this.closest('.modal').dispatchEvent(new Event('Close'))"
            ]) do
        span_ [ class_ "modal-title-bar " ] do
          bold (toHtml filename)
          div_ [ class_ "modal-title-bar-control " ] do
            case readOnly of
              True  -> bold (toHtml modal_readonly)
              False ->
                button_ [ class_ "btn modal-title-bar-confirm "
                        , type_ "submit"
                        ] (toHtml modal_edit)
            div_ [ class_ "btn-modal-close "
                 , term "data-close-modal" "true"
                 ] do
              i_ [ class_ "bx bx-x"] mempty
        input_ [ class_ "form-control ", type_ "hidden", name_ "path", value_ (txt_pack path) ]
        textarea_
          (mconcat
            [ [ class_ "form-control "
              , type_ "text"
              , name_ "content"
              , placeholder_ placeholder_empty_file
              , spellcheck_ "false"
              ]
            , if readOnly then [ readonly_ "readonly" ] else mempty
            ]
          )
          (toHtml (txt_decode content))


------------------------------------
-- elements
------------------------------------


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
  order    <- asks (.sortedBy)
  readOnly <- asks (.readOnly)
  ctx      <- ask

  emptyHtml <- Template.emptyDir
  pure do
    if null files
      then emptyHtml
      else table_ ([ id_ tableId, class_ "list-view ", term "data-sort-by" (sortByText order) ] <> [ term "data-readonly" "" | readOnly ]) do
        thead_ do
          tr_ do
            th_ do
              span_ [ class_ "field ", term "data-sort-field" "name" ] do
                toHtml detail_filename
                sortIconName order
            th_ do
              span_ [ class_ "field ", term "data-sort-field" "mtime" ] do
                toHtml detail_modified
                sortIconMTime order
            th_ do
              span_ [ class_ "field ", term "data-sort-field" "size" ] do
                toHtml detail_size
                sortIconSize order
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
  plugins             <- asks (.plugins)
  selected            <- asks (.selected)
  TargetView target _ <- asks (.currentTarget)

  let ClientPathView { clientPath, hashPath } = asClientPathView root file.path
      fileName = txt_pack (coerce takeFileName file.path)
      mtimeStr = maybe "" (txt_pack . time_format "%Y-%m-%dT%H:%M:%S") file.mtime
      atimeStr = maybe "" (txt_pack . time_format "%Y-%m-%dT%H:%M:%S") file.atime
      sizeStr  = txt_pack (show (fromMaybe 0 file.size))
      mimeStr  = txt_decode file.mimetype

  pure do
    tr_ do
      td_ [ class_ "entry-preview " ] do
        fileNameElement file target True
          `with` Template.open plugins root clientPath file
          `with`  [ class_ "field "]
      td_ $ modifiedDateElement file
      td_ $ sizeElement file
      `with`
        ( [ term "data-path" (txt_pack (coerce clientPath))
          , term "data-ftype" (fileType file)
          , term "data-name" fileName
          , term "data-mtime" mtimeStr
          , term "data-atime" atimeStr
          , term "data-size" sizeStr
          , term "data-mime" mimeStr
          , id_ [i|tr-#{hashPath}|]
          , class_ "table-item "
          , draggable_ "true"
          ]
        <> [ class_ "selected confirmed " | clientPath `Selected.elem` selected ]
        <> case file.content of
             Dir     -> [ class_ "dir " ]
             Regular -> []
        <> case file.isLink of
             Link       -> [ class_ "symlink " ]
             BrokenLink -> [ class_ "broken-symlink " ]
             NotLink    -> []
        )


thumbnailLayout :: [FileInfo] -> Template (Html ())
thumbnailLayout files = do
  readOnly  <- asks (.readOnly)
  order     <- asks (.sortedBy)
  ctx       <- ask
  emptyHtml <- Template.emptyDir
  pure do
    if null files
      then emptyHtml
      else div_ ([ id_ tableId, class_ "thumbnail-view ", term "data-sort-by" (sortByText order) ] <> [ term "data-readonly" "" | readOnly ]) do
        runTemplate ctx (entries files)


thumbnail :: FileInfo -> Template (Html ())
thumbnail file = do
  TargetView target _ <- asks (.currentTarget)
  root                <- asks (.root)
  plugins             <- asks (.plugins)
  selected            <- asks (.selected)

  let ClientPathView { clientPath, hashPath } = asClientPathView root file.path
      fileName = txt_pack (coerce takeFileName file.path)
      mtimeStr = maybe "" (txt_pack . time_format "%Y-%m-%dT%H:%M:%S") file.mtime
      atimeStr = maybe "" (txt_pack . time_format "%Y-%m-%dT%H:%M:%S") file.atime
      sizeStr  = txt_pack (show (fromMaybe 0 file.size))
      mimeStr  = txt_decode file.mimetype

  pure $
    (div_ do
      previewElement clientPath file
      fileNameElement file target False `with` [ class_ "thumbnail-name" ])
    `with`
      ( [ term "data-path" (txt_pack (coerce clientPath))
        , term "data-ftype" (fileType file)
        , term "data-name" fileName
        , term "data-mtime" mtimeStr
        , term "data-atime" atimeStr
        , term "data-size" sizeStr
        , term "data-mime" mimeStr
        , id_ [i|tr-#{hashPath}|]
        , class_ "thumbnail table-item "
        , draggable_ "true"
        ]
      <> [ class_ "selected confirmed " | clientPath `Selected.elem` selected ]
      <> case file.content of
           Dir     -> [ class_ "dir " ]
           Regular -> []
      <> case file.isLink of
           Link       -> [ class_ "symlink " ]
           BrokenLink -> [ class_ "broken-symlink " ]
           NotLink    -> []
      )
    `with` Template.open plugins root clientPath file


previewElement :: ClientPath -> FileInfo -> Html ()
previewElement (ClientPath cp) file = do
  div_ [ class_ "thumbnail-preview " ] do
    div_ [  class_ "image-wrapper " ] do
      let serveUrl = "/serve?file=" <> txt_pack cp
      if
         | file.mimetype `isMime` "image" ->
           img_ [ src_ serveUrl
                , draggable_ "false"
                , term "loading" "lazy"
                , term "fetchpriority" "low"
                ]
         | file.mimetype `isMime` "video" ->
           video_ [ src_ serveUrl
                  , term "muted" ""
                  , term "preload" "metadata"
                  , term "loop" ""
                  , term "playsinline" ""
                  , class_ "thumbnail-video"
                  , draggable_ "false"
                  ] mempty
         | otherwise -> Template.icon file


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
        let item :: Html () -> Html () -> Text -> Html ()
            item label ico field = div_ [ class_ "dropdown-item ", term "data-sort-field" field ] do
              ico
              span_ label
        item (toHtml detail_filename) (sortIconName order)  "name"
        item (toHtml detail_modified) (sortIconMTime order) "mtime"
        item (toHtml detail_size)     (sortIconSize order)  "size"


------------------------------------
-- component ids
------------------------------------


editorModalId :: Text
editorModalId = "editor-modal"
