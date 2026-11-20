{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template.Mobile
  ( index
  , sideBar
  , controlPanel
  , view
  , entry
  , entries
  , table
  , toolBar
  , editorModal
  )
  where

import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.ClientPath (ClientPath(..), AbsPath (..), Root (..))
import Data.File (File(..), FileInfo, IsLink (..))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (iii, i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Format (formatTime, defaultTimeLocale)
import Filehub.Locale (Phrase(..), phrase, Locale (..))
import Filehub.Routes (Api(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Size (toReadableSize)
import Filehub.Template (Template, TemplateContext(..), runTemplate)
import Filehub.Template.Shared qualified as Template
import Filehub.Template.Shared
  ( sideBarId, viewId, searchBar, toolBarId, tableId
  , fileNameElement
  , sortIconName, sortIconMTime, sortIconSize, sortByText
  )
import Filehub.Theme (Theme(..))
import Lucid
import System.FilePath (takeFileName)
import Target.File (Target (..), FileSys)
import Target.S3 (Target (..), S3)
import Target.Types (targetHandler, AnyTarget, handleTarget)
import Target.Types qualified as Target
import Filehub.Session (TargetView(..))
import Target.Dummy (DummyTarget)
import Data.Coerce (coerce)
import Control.Monad.Reader (asks, MonadReader (..))
import Data.ClientPath.View (ClientPathView(..), AsClientPathView (..))
import Lucid.Htmx (hxGet, hxPost, hxTrigger, hxEncoding, Trigger (..), hxConfirm, hxDelete, HtmxEvent (..), hxOn)
import Filehub.Template.Htmx (asHtmx)


index :: Html ()
      -> Html ()
      -> Html ()
      -> Int
      -> Template (Html ())
index sideBar' toolBar' view' selectedCount = do
  controlPanel' <- controlPanel
  pure do
    safeAreaShim
    div_ [ id_ "index" ] do
      overlay
      selectedCounter selectedCount
      sideBar'
      toolBar'
      view'
      controlPanel'
      languagePanel


safeAreaShim :: Html ()
safeAreaShim = div_ [ id_ "safe-area-shim" ] mempty


overlay :: Html ()
overlay = div_ [ id_ overlayId ] mempty


sideBar :: [AnyTarget] -> TargetView -> Html ()
sideBar targets (TargetView currentTarget _) = do
  div_ [ id_ sideBarId ] do traverse_ targetIcon targets
  where
    targetIcon :: AnyTarget -> Html ()
    targetIcon target = do
      div_ (asHtmx @"changeTarget" hxGet (Just (Target.getTargetId target))
           [ class_ "target-icon" ]) do
        fromMaybe "unknown" $ handleTarget target
          [ targetHandler @FileSys \(FileBackend { root = Root (AbsPath root) }) -> do
              i_ [ class_ "bx bx-folder" ] mempty
              span_ [iii| /#{takeFileName root} |]
          , targetHandler @S3 \(S3Backend { bucket }) -> do
              i_ [ class_ "bx bxs-cube" ] mempty
              span_  [iii| /#{bucket} |]
          , targetHandler @DummyTarget \_ -> do
              i_ [ class_ "bx bxs-cube" ] mempty
              span_  [iii| dummy |]
          ]
      `with` targetAttr
      where
        targetAttr = [class_ " current-target" | Target.getTargetId currentTarget == Target.getTargetId target]


controlPanelBtn :: Html ()
controlPanelBtn =
  button_ [ id_ controlPanelBtnId ] do
    i_ [ class_ "bx bx-plus" ] mempty


view :: Html () -> Html ()
view table' = do
  div_ [ id_ viewId ] do
    table'
  controlPanelBtn


sidebarBtn :: Html ()
sidebarBtn =
  button_ [ id_ sidebarBtnId ] do
    i_ [ class_ "bx bx-menu" ] mempty


toolBar :: Template (Html ())
toolBar = do
  sortTool'       <- sortTool
  pathBreadcrumb' <- Template.pathBreadcrumb
  searchBar'      <- searchBar
  pure do
    div_ [ id_ toolBarId ] do
      div_ do
        sidebarBtn
        searchBar'
      div_ do
        pathBreadcrumb'
        sortTool'


sortTool :: Template (Html ())
sortTool = do
  order <- asks (.sortedBy)
  Phrase
    { detail_filename
    , detail_modified
    , detail_size
    } <- phrase <$> asks (.locale)

  pure do
    div_ [ id_ sortControlId ] do
      span_ [ class_ "field ", term "data-sort-field" "name" ] do
        (toHtml detail_filename)
        sortIconName order
      span_ [ class_ "field ", term "data-sort-field" "mtime" ] do
        (toHtml detail_modified)
        sortIconMTime order
      span_ [ class_ "field ", term "data-sort-field" "size" ] do
        (toHtml detail_size)
        sortIconSize order


table :: [FileInfo] -> Template (Html ())
table files = do
  ctx <- ask
  order <- asks (.sortedBy)
  pure do
    table_ [ id_ tableId, class_ "list-view ", term "data-sort-by" (sortByText order) ] do
      tbody_ $ runTemplate ctx (entries files)


entries :: [FileInfo] -> Template (Html ())
entries files = mconcat <$> traverse entry files


entry :: FileInfo -> Template (Html ())
entry file = do
  root                  <- asks (.root)
  plugins               <- asks (.plugins)
  TargetView { target } <- asks (.currentTarget)
  selected              <- asks (.selected)

  let fileName = Text.pack (coerce takeFileName file.path)
      mtimeStr = maybe "" (Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S") file.mtime
      atimeStr = maybe "" (Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S") file.atime
      sizeStr  = Text.pack (show (fromMaybe 0 file.size))
      mimeStr  = Text.decodeUtf8 file.mimetype
      attrs :: [Attribute]
      attrs = mconcat
        [ [ term "data-path" (Text.pack (coerce clientPath)) ]
        , [class_ "selected " | clientPath `Selected.elem` selected]
        , [id_ [i|tr-#{hashPath}|], class_ "table-item " ]
        , [ term "data-name" fileName
          , term "data-mtime" mtimeStr
          , term "data-atime" atimeStr
          , term "data-size" sizeStr
          , term "data-mime" mimeStr
          ]
        ]
      ClientPathView { clientPath, hashPath  } = asClientPathView root file.path

  pure do
    tr_ attrs do
      td_  do
        fileNameElement file target True `with` [class_ "field"]
        span_ [class_ "file-meta mobile "] do
          modifiedDateElement file
          i_ [ class_ "bx bx-wifi-0"] mempty
          sizeElement file
        `with` Template.open plugins root file
        `with` [ class_ "entry-preview " ]
      `with` [ case file.isLink of
                 Link       -> class_ "symlink "
                 BrokenLink -> class_ "broken-symlink "
                 NotLink    -> class_ ""
             ]


sizeElement :: FileInfo -> Html ()
sizeElement file =
  span_ (toHtml displaySize)
    `with` [ class_ "field "
           , title_ (Text.pack displaySize)
           ]
  where
    displaySize = toReadableSize $ fromMaybe 0 file.size


modifiedDateElement :: FileInfo -> Html ()
modifiedDateElement file =
  span_ (toHtml displayTime)
    `with` [ class_ "field "
           , title_ (Text.pack displayTime)
           ]
  where
    displayTime = maybe mempty (formatTime defaultTimeLocale "%Y/%m/%d") file.mtime


languagePanel :: Html ()
languagePanel =
  div_ [ id_ "locale", class_ "panel " ] do
    let item :: Locale -> Html () -> Html ()
        item loc label = div_ (asHtmx @"changeLocale" hxGet (Just loc)
                              [ class_ "dropdown-item" ]) do span_ label
    item EN "English"
    item ZH_CN "简体中文"
    item ZH_TW "繁體中文"
    item ZH_HK "繁體中文"
    item JA "日本語"
    item ES "Español"
    item FR "Français"
    item DE "Deutsch"
    item KO "한국어"
    item RU "Русский"
    item PT "Português"
    item IT "Italiano"


controlPanel :: Template (Html ())
controlPanel = (fmap (`with` [ class_ "panel "]) . join) do
  Template.controlPanel
    <$> localeBtn
    <*> newFolderBtn
    <*> newFileBtn
    <*> uploadBtn
    <*> copyBtn
    <*> pasteBtn
    <*> deleteBtn
    <*> cancelBtn
    <*> themeBtn
    <*> logoutBtn
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> (Just <$> scroll2TopBtn)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
  where
    localeBtn :: Template (Html ())
    localeBtn = do
      Phrase { control_panel_language } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , id_ "locale-langauge-btn"
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bx-world" ] mempty
            span_ (toHtml control_panel_language)


    newFolderBtn :: Template (Html ())
    newFolderBtn = do
      Phrase { control_panel_new_folder } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , id_ "new-folder-btn"
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bx-folder-plus" ] mempty
            span_ (toHtml control_panel_new_folder)


    newFileBtn :: Template (Html ())
    newFileBtn  = do
      Phrase { control_panel_new_file } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , id_ "new-file-btn"
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-file-plus" ] mempty
            span_ (toHtml control_panel_new_file)


    uploadBtn :: Template (Html ())
    uploadBtn = do
      Phrase { control_panel_upload } <- phrase <$> asks (.locale)
      let fileInputId = "file-input"
      pure do
        input_ (asHtmx @"upload" hxPost ()
               [ type_ "file"
               , name_ "file"
               , id_ fileInputId
               , style_ "display:none"
               , hxEncoding "multipart/form-data"
               , hxTrigger Change
               ])

        button_ [ class_ "action-btn"
                , onclick_ [iii|document.querySelector('\##{fileInputId}').click()|]
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bx-upload" ] mempty
            span_ (toHtml control_panel_upload)


    copyBtn :: Template (Html ())
    copyBtn = do
      Phrase { control_panel_copy } <- phrase <$> asks (.locale)
      pure do
        button_ (asHtmx @"copy" hxGet () [ class_ "action-btn" ]) do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-copy-alt" ] mempty
            span_ (toHtml control_panel_copy)


    pasteBtn :: Template (Html ())
    pasteBtn = do
      Phrase { control_panel_paste } <- phrase <$> asks (.locale)
      pure do
        button_ (asHtmx @"paste" hxPost () [ class_ "action-btn" ]) do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-paste" ] mempty
            span_ (toHtml control_panel_paste)


    deleteBtn :: Template (Html ())
    deleteBtn = do
      selected <- asks (.selected)
      Phrase { control_panel_delete } <- phrase <$> asks (.locale)
      pure do
        button_ (asHtmx @"delete" hxDelete ((Selected.toList selected), True)
                [ class_ "action-btn urgent "
                , hxConfirm ("Are you sure about deleting selected files?")
                ]) do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-trash" ] mempty
            span_ (toHtml control_panel_delete)


    cancelBtn :: Template (Html ())
    cancelBtn = do
      Phrase { control_panel_cancel } <- phrase <$> asks (.locale)
      pure do
        button_ (asHtmx @"cancel" hxPost () [ class_ "action-btn" ]) do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-message-alt-x" ] mempty
            span_ (toHtml control_panel_cancel)


    logoutBtn :: Template (Html ())
    logoutBtn = do
      Phrase { control_panel_logout } <- phrase <$> asks (.locale)
      pure do
        button_ (asHtmx @"logout" hxPost ()
                [ class_ "action-btn urgent "
                , type_ "submit"
                , hxConfirm "Logout?"
                ]) do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bx-power-off" ] mempty
            span_ (toHtml control_panel_logout)


    themeBtn :: Template (Html ())
    themeBtn = do
      theme <- asks (.theme)
      Phrase
        { control_panel_light
        , control_panel_dark } <- phrase <$> asks (.locale)
      pure do
        case theme of
          Light -> do
            button_ (asHtmx @"toggleTheme" hxGet ()
                    [ class_ "action-btn"
                    , type_ "submit"
                    ]) do
              i_ [ class_ "bx bxs-moon" ] mempty
              span_ (toHtml control_panel_dark )
          Dark -> do
            button_ (asHtmx @"toggleTheme" hxGet ()
                    [ class_ "action-btn"
                    , type_ "submit"
                    ]) do
              i_ [ class_ "bx bxs-sun" ] mempty
              span_ (toHtml control_panel_light )


    scroll2TopBtn :: Template (Html ())
    scroll2TopBtn = do
      Phrase { control_panel_scroll2top } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , id_ "scroll2top-btn"
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bx-vertical-top" ] mempty
            span_ (toHtml control_panel_scroll2top)


selectedCounter :: Int -> Html ()
selectedCounter n = do
  div_ (asHtmx @"cancel" hxPost ()
       [ id_ selectedCounterId
       , class_ "field "
       ]) do
    span_ [i|#{n}|]
    span_ "selected"
    i_ [ class_ "bx bx-x" ] mempty


editorModal :: (ClientPath, String) -> ByteString -> Template (Html ())
editorModal (ClientPath path, filename) content = do
  readOnly <- asks (.readOnly)
  Phrase
    { modal_edit
    , confirm_save_edit
    } <- phrase <$> asks (.locale)


  pure do
    div_ [ id_ editorModalId ] do
      form_ (asHtmx @"updateFile" hxPost ()
            [ hxConfirm (Text.replace "{}" (Text.pack filename) confirm_save_edit)
            , hxOn AfterRequest "document.getElementById('editor-modal').remove()"
            ]) do
        div_ do
          button_ [ class_ "btn btn-modal-close "
                  , type_ "button"
                  , term "data-close-editor" "true"
                  ] do
            span_ [ class_ "field "] do
              i_ [ class_ "bx bx-chevron-left" ] mempty

          case readOnly of
            True ->
              mempty

            False -> do
              button_ [ class_ "btn btn-modal-confirm mr-2 field "
                      , type_ "submit"
                      ] (toHtml modal_edit)

        input_ [ class_ "form-control "
               , type_ "text"
               , name_ "path"
               , value_ (Text.pack path)
               , style_ "display: none;"
               , placeholder_ "Filename"
               ]

        textarea_
          (mconcat
            [ [ class_ "form-control "
              , type_ "text"
              , name_ "content"
              , placeholder_ "Empty File"
              ]
            , if readOnly then [ readonly_ "readonly" ] else mempty
            ]
          )
          (toHtml (Text.decodeUtf8 content))


------------------------------------
-- component ids
------------------------------------

sidebarBtnId :: Text
sidebarBtnId = "sidebar-btn"

controlPanelBtnId :: Text
controlPanelBtnId = "control-panel-btn"

sortControlId :: Text
sortControlId = "sort-control"

selectedCounterId :: Text
selectedCounterId = "selected-counter"

editorModalId :: Text
editorModalId = "editor-modal"

overlayId :: Text
overlayId = "overlay"
