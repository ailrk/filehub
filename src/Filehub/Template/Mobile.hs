{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template.Mobile
  ( index
  , sideBar
  , controlPanel
  , view
  , entry
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
import Filehub.Links ( apiLinks, linkToText )
import Filehub.Locale (Phrase(..), phrase, Locale (..))
import Filehub.Routes (Api(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Size (toReadableSize)
import Filehub.Template (Template, TemplateContext(..), runTemplate)
import Filehub.Template.Shared qualified as Template
import Filehub.Template.Shared (sideBarId, controlPanelId, viewId, searchBar, toolBarId, tableId)
import Filehub.Theme (Theme(..))
import Filehub.Types ( SortFileBy(..))
import Lens.Micro.Platform ()
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
import Lucid.Htmx (Swap(..), HxSwap (..), hxTarget, hxGet, HxTrigger (..), HxPost (..), hxEncoding, Trigger (..), hxConfirm, hxDelete, HtmxEvent (..), HxOn (..))


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
      div_ [ class_ "target-icon"
           , hxGet (apiLinks.changeTarget (Just (Target.getTargetId target)))
           , hxTarget "#index"
           , hxSwap OuterHTML
           ] do
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
  button_ [ id_ controlPanelBtnId
          , term "_" [i|on click toggle .show on \##{overlayId} toggle .show on \##{controlPanelId}|]
          ] do
    i_ [ class_ "bx bx-plus" ] mempty


view :: Html () -> Html ()
view table' = do
  div_ [ id_ viewId ] do
    table'
  controlPanelBtn


sidebarBtn :: Html ()
sidebarBtn =
  button_ [ id_ sidebarBtnId
          , term "_" [i|on click toggle .show on \##{overlayId} wait 50ms toggle .show on \##{sideBarId}|]
          ] do
    i_ [ class_ "bx bx-menu" ] mempty


toolBar :: Template (Html ())
toolBar = do
  sortTool' <- sortTool
  pathBreadcrumb' <- Template.pathBreadcrumb
  searchBar' <- searchBar
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
      span_ [ class_ "field " ] do
        (toHtml detail_filename)
        sortIconName order
        `with` sortControlName order
      span_ [ class_ "field " ] do
        (toHtml detail_modified)
        sortIconMTime order
        `with` sortControlMTime order
      span_ [ class_ "field " ] do
        (toHtml detail_size)
        sortIconSize order
        `with` sortControlSize order
  where
    sortControl o =
      [ hxGet (apiLinks.sortTable (Just o))
      , hxSwap OuterHTML
      , hxTarget "#index"
      ]
    sortControlName = \case
        ByNameUp   -> sortControl ByNameDown
        ByNameDown -> sortControl ByNameUp
        _          -> sortControl ByNameUp
    sortControlMTime = \case
        ByModifiedUp   -> sortControl ByModifiedDown
        ByModifiedDown -> sortControl ByModifiedUp
        _              -> sortControl ByModifiedUp
    sortControlSize = \case
        BySizeUp   -> sortControl BySizeDown
        BySizeDown -> sortControl BySizeUp
        _          -> sortControl BySizeUp
    sortIconName = \case
        ByNameUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
        ByNameDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
        _          -> i_ [ class_ "bx bx-sort"] mempty
    sortIconMTime = \case
        ByModifiedUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
        ByModifiedDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
        _              -> i_ [ class_ "bx bx-sort"] mempty
    sortIconSize = \case
        BySizeUp   -> i_ [ class_ "bx bxs-up-arrow"] mempty
        BySizeDown -> i_ [ class_ "bx bxs-down-arrow"] mempty
        _          -> i_ [ class_ "bx bx-sort"] mempty


table :: [FileInfo] -> Template (Html ())
table files = do
  ctx <- ask
  pure do
    table_ [ id_ tableId, class_ "list-view " ] do
      tbody_ $ traverse_ (runTemplate ctx . entry) files


entry :: FileInfo -> Template (Html ())
entry file = do
  root                  <- asks (.root)
  TargetView { target } <- asks (.currentTarget)
  selected              <- asks (.selected)

  let attrs :: [Attribute]
      attrs = mconcat
        [ [ term "data-path" (Text.pack (coerce clientPath)) ]
        , [class_ "selected " | clientPath `Selected.elem` selected]
        , [id_ [i|tr-#{hashPath}|], class_ "table-item " ]
        ]
      ClientPathView { clientPath, hashPath  } = asClientPathView root file.path

  pure do
    tr_ attrs do
      td_  do
        fileNameElement target file
        span_ [class_ "file-meta mobile "] do
          modifiedDateElement file
          i_ [ class_ "bx bx-wifi-0"] mempty
          sizeElement file
        `with` Template.open root file
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


fileNameElement :: AnyTarget -> FileInfo -> Html ()
fileNameElement target file = do
  span_ (Template.icon file >> name)
    `with` [ class_ "field"
           , title_ (Text.pack displayName)
           ]
  where
    name = span_ (toHtml displayName)
    displayName =
      fromMaybe "-" $ handleTarget target
        [ targetHandler @S3          \_ -> coerce file.path
        , targetHandler @FileSys     \_ -> coerce takeFileName file.path
        , targetHandler @DummyTarget \_ -> coerce takeFileName file.path
        ]


languagePanel :: Html ()
languagePanel =
  div_ [ id_ "locale", class_ "panel " ] do
    let item :: Locale -> Html () -> Html ()
        item loc label = div_ [ class_ "dropdown-item"
                              , hxGet (apiLinks.loginChangeLocale (Just loc))
                              , hxTarget "#login"
                              , hxSwap OuterHTML
                              ] do span_ label
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
  where
    localeBtn :: Template (Html ())
    localeBtn = do
      Phrase { control_panel_language } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , id_ "locale-langauge-btn"
                , term "_" [i|on click toggle .show on \#locale|]
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bx-world" ] mempty
            span_ (toHtml control_panel_language)


    newFolderBtn :: Template (Html ())
    newFolderBtn = do
      Phrase { control_panel_new_folder } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , term "_"
                    [iii|
                      on click
                        set name to prompt('New folder')
                        if name is not null
                        then call
                          htmx.ajax('POST',
                                    '/folders/new',
                                    { target: '\##{viewId}',
                                      values: {'new-folder': name},
                                      swap: 'outerHTML'
                                    })
                      |]
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bx-folder-plus" ] mempty
            span_ (toHtml control_panel_new_folder)


    newFileBtn :: Template (Html ())
    newFileBtn  = do
      Phrase { control_panel_new_file } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , term "_"
                    [iii|
                      on click
                        set name to prompt('New file')
                        if name is not null
                        then call
                          htmx.ajax('POST',
                                    '/files/new',
                                    { target: '\##{viewId}',
                                      values: {'new-file': name},
                                      swap: 'outerHTML'
                                    })
                      |]
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-file-plus" ] mempty
            span_ (toHtml control_panel_new_file)


    uploadBtn :: Template (Html ())
    uploadBtn = do
      Phrase { control_panel_upload } <- phrase <$> asks (.locale)
      let fileInputId = "file-input"
      pure do
        input_ [ type_ "file"
               , name_ "file"
               , id_ fileInputId
               , style_ "display:none"
               , hxEncoding "multipart/form-data"
               , hxPost apiLinks.upload
               , hxTarget "#index"
               , hxSwap OuterHTML
               , hxTrigger Change
               ]

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
        button_ [ class_ "action-btn"
                , hxGet apiLinks.copy
                , hxTarget "#control-panel"
                , hxSwap OuterHTML
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-copy-alt" ] mempty
            span_ (toHtml control_panel_copy)


    pasteBtn :: Template (Html ())
    pasteBtn = do
      Phrase { control_panel_paste } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , hxPost apiLinks.paste
                , hxSwap None
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-paste" ] mempty
            span_ (toHtml control_panel_paste)


    deleteBtn :: Template (Html ())
    deleteBtn = do
      selected <- asks (.selected)
      Phrase { control_panel_delete } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn urgent "
                , hxDelete (apiLinks.delete (Selected.toList selected) True)
                , hxSwap None
                , hxConfirm ("Are you sure about deleting selected files?")
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-trash" ] mempty
            span_ (toHtml control_panel_delete)


    cancelBtn :: Template (Html ())
    cancelBtn = do
      Phrase { control_panel_cancel } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , hxPost apiLinks.cancel
                , hxTarget "#index"
                , hxSwap OuterHTML
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bxs-message-alt-x" ] mempty
            span_ (toHtml control_panel_cancel)


    logoutBtn :: Template (Html ())
    logoutBtn = do
      Phrase { control_panel_logout } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn urgent "
                , type_ "submit"
                , hxPost (linkToText apiLinks.logout)
                , hxTarget "#index"
                , hxSwap OuterHTML
                , hxConfirm "Logout?"
                ] do
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
            button_ [ class_ "action-btn"
                    , type_ "submit"
                    , hxGet apiLinks.toggleTheme
                    , hxTarget "#index"
                    , hxSwap OuterHTML
                    ] do
              i_ [ class_ "bx bxs-moon" ] mempty
              span_ (toHtml control_panel_dark )
          Dark -> do
            button_ [ class_ "action-btn"
                    , type_ "submit"
                    , hxGet apiLinks.toggleTheme
                    , hxTarget "#index"
                    , hxSwap OuterHTML
                    ] do
              i_ [ class_ "bx bxs-sun" ] mempty
              span_ (toHtml control_panel_light )


    scroll2TopBtn :: Template (Html ())
    scroll2TopBtn = do
      Phrase { control_panel_scroll2top } <- phrase <$> asks (.locale)
      pure do
        button_ [ class_ "action-btn"
                , term "_" "on click call window.scroll(0, 0)"
                ] do
          span_ [ class_ "field " ] do
            i_ [ class_ "bx bx-vertical-top" ] mempty
            span_ (toHtml control_panel_scroll2top)


selectedCounter :: Int -> Html ()
selectedCounter n = do
  div_ [ id_ selectedCounterId
       , hxPost (linkToText apiLinks.cancel)
       , hxTarget "#index"
       , hxSwap OuterHTML
       , class_ "field "
       ] do
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
    div_ [ id_ editorModalId, closeEditorScript ] do

      form_ [ hxPost (apiLinks.updateFile)
            , hxConfirm (Text.replace "{}" (Text.pack filename) confirm_save_edit)
            , hxOn AfterRequest [i|document.querySelector('\##{editorModalId}').dispatchEvent(new Event('Close'))|]
            ] do

        div_ do
          button_ [ class_ "btn btn-modal-close "
                  , type_ "button"
                  , term "_" [i|on click send Close to \##{editorModalId}|]
                  ] do
            span_ [ class_ "field "] do
              i_ [ class_ "bx bx-chevron-left" ] mempty

          case readOnly of
            True ->
              mempty

            False -> do
              button_ [ class_ "btn btn-modal-confirm mr-2 field " ] (toHtml modal_edit)

        input_ [ class_ "form-control "
               , type_ "text"
               , name_ "path"
               , value_ (Text.pack path)
               , style_ "display: none;"
               , placeholder_ "Filename"
               ]

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
          (toHtml (Text.decodeUtf8 content))

  where
    closeEditorScript = term "_" [i| on Close remove me end |]


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
