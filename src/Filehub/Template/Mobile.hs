{-# LANGUAGE NamedFieldPuns #-}

module Filehub.Template.Mobile where


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
      SearchWord(..),
      SortFileBy(..),
      ClientPath(..),
      Target(..),
      Selected,
      ControlPanelState(..) )
import Filehub.Sort (sortFiles)
import Filehub.Size (toReadableSize)
import Filehub.Selected qualified as Selected
import Filehub.ClientPath qualified as ClientPath
import Filehub.Routes (Api(..))
import Filehub.Target (TargetView(..), handleTarget)
import Filehub.Target qualified as Target
import Filehub.Template.Internal (viewId, sideBarId, controlPanelId, toolBarId, tableId, searchBar)
import Filehub.Template.Internal qualified as Template
import Filehub.Links ( apiLinks, linkToText )
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import System.FilePath (takeFileName)
import Text.Fuzzy (simpleFilter)
import Filehub.Target.File (Backend (..), FileSys)
import Filehub.Target.S3 (Backend (..), S3)
import Filehub.Target.Types (targetHandler)
import Filehub.Theme (Theme(..))


index :: Bool
      -> Bool
      -> Html ()
      -> Html ()
      -> Theme
      -> ControlPanelState
      -> Int
      -> Html ()
index readOnly noLogin sideBar' view' theme controlPanelState selectedCount = do
  safeAreaShim
  div_ [ id_ "index" ] do
    overlay
    selectedCounter selectedCount
    sideBar'
    view'
    controlPanel theme readOnly noLogin controlPanelState
    controlPanelBtn


safeAreaShim :: Html ()
safeAreaShim = div_ [ id_ "safe-area-shim" ] mempty


overlay :: Html ()
overlay = div_ [ id_ overlayId ] mempty


sideBar :: [Target] -> TargetView -> Html ()
sideBar targets (TargetView currentTarget _ _) = do
  div_ [ id_ sideBarId ] do traverse_ targetIcon targets

  where
    targetIcon :: Target -> Html ()
    targetIcon target = do
      div_ [ class_ "target-icon"
           , term "hx-get" $ linkToText (apiLinks.changeTarget (Just (Target.getTargetId target)))
           , term "hx-target" "#index"
           , term "hx-swap" "outerHTML"
           ] do
        fromMaybe "unknown" $ handleTarget target
          [ targetHandler @FileSys $ \(FileBackend { root }) -> do
              i_ [ class_ "bx bx-folder" ] mempty
              span_ [iii| /#{takeFileName root} |]
          , targetHandler @S3 $ \(S3Backend { bucket }) -> do
              i_ [ class_ "bx bxs-cube" ] mempty
              span_  [iii| /#{bucket} |]
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


view :: Html () -> Html () -> Html () -> Html ()
view table' sortTool' pathBreadcrumb' = do
  div_ [ id_ viewId ] do
    toolBar sortTool' pathBreadcrumb'
    table'


sidebarBtn :: Html ()
sidebarBtn =
  button_ [ id_ sidebarBtnId
          , term "_" [i|on click toggle .show on \##{overlayId} wait 50ms toggle .show on \##{sideBarId}|]
          ] do
    i_ [ class_ "bx bx-menu" ] mempty


toolBar :: Html () -> Html () -> Html ()
toolBar sortTool' pathBreadcrumb' = do
  div_ [ id_ toolBarId ] do
    div_ do
      sidebarBtn
      searchBar
    div_ do
      pathBreadcrumb'
      sortTool'


search :: SearchWord -> Target -> FilePath -> [File] -> Selected -> SortFileBy -> Html ()
search (SearchWord searchWord) target root files selected order = do
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  table target root (sortFiles order filteredFiles) selected


sortTool :: SortFileBy -> Html ()
sortTool order = do
  div_ [ id_ sortControlId ] do
    span_ [ class_ "field " ] do
      "Name"
      sortIconName
      `with` sortControlName
    span_ [ class_ "field " ] do
      "Time"
      sortIconMTime
      `with` sortControlMTime
    span_ [ class_ "field " ] do
      "Size"
      sortIconSize
      `with` sortControlSize
  where
    sortControl o =
      [ term "hx-get" $ linkToText (apiLinks.sortTable (Just o))
      , term "hx-swap" "outerHTML"
      , term "hx-target" "#view"
      ]

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


table :: Target -> FilePath -> [File] -> Selected -> Html ()
table target root files selected = do
  table_ [ id_ tableId, class_ "list-view " ] do
    tbody_ $ traverse_ record ([0..] `zip` files)
  where
    record :: (Int, File) -> Html ()
    record (idx, file) =
      tr_ attrs do
        td_ do
          fileNameElement file
          span_ [class_ "file-meta"] do
            modifiedDateElement file
            i_ [ class_ "bx bx-wifi-0"] mempty
            sizeElement file
          `with` Template.open root file
      where
        attrs :: [Attribute]
        attrs = mconcat
          [ [ term "data-path" (Text.pack path) ]
          , [class_ "selected " | clientPath `Selected.elem` selected]
          , [id_ [i|tr-#{idx}|], class_ "table-item " ]
          ]
        clientPath@(ClientPath path) = ClientPath.toClientPath root file.path

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
        displayTime = maybe mempty (formatTime defaultTimeLocale "%Y/%m/%d") file.mtime

    fileNameElement :: File -> Html ()
    fileNameElement file = do
      span_ (Template.icon file >> name)
        `with` [ class_ "field"
               , title_ (Text.pack displayName)
               ]
      where
        name = span_ (toHtml displayName)

        displayName =
          fromMaybe "-" $ handleTarget target
            [ targetHandler @S3 $ \_ -> file.path
            , targetHandler @FileSys $ \_ -> takeFileName file.path
            ]


controlPanel :: Theme -> Bool -> Bool -> ControlPanelState -> Html ()
controlPanel theme =
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
    Nothing
    (Just scroll2TopBtn)
  where
    newFolderBtn :: Html ()
    newFolderBtn =
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
          span_ "New Folder"


    newFileBtn :: Html ()
    newFileBtn  =
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

      button_ [ class_ "action-btn"
              , onclick_ [iii|document.querySelector('\##{fileInputId}').click()|]
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bx-upload" ] mempty
          span_ "Upload"


    copyBtn :: Html ()
    copyBtn = do
      button_ [ class_ "action-btn"
              , term "hx-get" $ linkToText apiLinks.copy
              , term "hx-target" "#control-panel"
              , term "hx-swap" "outerHTML"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-copy-alt" ] mempty
          span_ "Copy"


    pasteBtn :: Html ()
    pasteBtn = do
      button_ [ class_ "action-btn"
              , term "hx-post" $ linkToText apiLinks.paste
              , term "hx-target" "#index"
              , term "hx-swap" "outerHTML"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-paste" ] mempty
          span_ "Paste"


    deleteBtn :: Html ()
    deleteBtn = do
      button_ [ class_ "action-btn urgent "
              , term "hx-delete" $ linkToText (apiLinks.deleteFile [] True)
              , term "hx-target" "#index"
              , term "hx-swap" "outerHTML"
              , term "hx-confirm" ("Are you sure about deleting selected files?")
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-trash" ] mempty
          span_ "Delete"


    cancelBtn :: Html ()
    cancelBtn = do
      button_ [ class_ "action-btn"
              , term "hx-post" $ linkToText apiLinks.cancel
              , term "hx-target" "#index"
              , term "hx-swap" "outerHTML"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-message-alt-x" ] mempty
          span_ "Cancel"


    logoutBtn :: Html ()
    logoutBtn = do
      button_ [ class_ "action-btn urgent "
              , type_ "submit"
              , term "hx-post" $ linkToText apiLinks.logout
              , term "hx-target" "#index"
              , term "hx-swap" "outerHTML"
              , term "hx-confirm" "Logout?"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bx-power-off" ] mempty
          span_ "Logout"


    themeBtn :: Html ()
    themeBtn = do
      case theme of
        Light -> do
          button_ [ class_ "action-btn"
                  , type_ "submit"
                  , term "hx-get" $ linkToText apiLinks.toggleTheme
                  , term "hx-target" "#index"
                  , term "hx-swap" "outerHTML"
                  ] do
            i_ [ class_ "bx bxs-moon" ] mempty
            span_ "Switch to dark mode"
        Dark -> do
          button_ [ class_ "action-btn"
                  , type_ "submit"
                  , term "hx-get" $ linkToText apiLinks.toggleTheme
                  , term "hx-target" "#index"
                  , term "hx-swap" "outerHTML"
                  ] do
            i_ [ class_ "bx bxs-sun" ] mempty
            span_ "Switch to light mode"


    scroll2TopBtn :: Html ()
    scroll2TopBtn = do
      button_ [ class_ "action-btn"
              , term "_" "on click call window.scroll(0, 0)"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bx-vertical-top" ] mempty
          span_ "Back to top"


selectedCounter :: Int -> Html ()
selectedCounter n = do
  div_ [ id_ selectedCounterId
       , term "hx-post" $ linkToText apiLinks.cancel
       , term "hx-target" "#index"
       , term "hx-swap" "outerHTML"
       , class_ "field "
       ] do
    span_ [i|#{n}|]
    span_ "selected"
    i_ [ class_ "bx bx-x" ] mempty


editorModal :: Bool -> FilePath -> ByteString -> Html ()
editorModal readOnly filename content = do
  div_ [ id_ editorModalId, closeEditorScript ] do

    form_ [ term "hx-post" $ linkToText (apiLinks.updateFile)
          , term "hx-confirm" ("Save the edit of " <> Text.pack filename <> "?")
          , term "hx-on::after-request" [i|document.querySelector('\##{editorModalId}').dispatchEvent(new Event('Close'))|]
          ] do

      div_ do
        button_ [ class_ "btn btn-modal-close "
                , type_ "button"
                , term "_" [i|on click send Close to \##{editorModalId}|]
                ] do
          span_ [ class_ "field "] do
            i_ [ class_ "bx bx-chevron-left" ] mempty
            "Folders"

        case readOnly of
          True ->
            mempty

          False -> do
            button_ [ class_ "btn btn-modal-confirm mr-2 field "
                    ] "DONE"

      input_ [ class_ "form-control "
             , type_ "text"
             , name_ "path"
             , value_ (Text.pack filename)
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
        (toHtml $ Text.decodeUtf8 content)

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
