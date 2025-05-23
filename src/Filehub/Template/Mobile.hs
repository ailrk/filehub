{-# LANGUAGE NamedFieldPuns #-}

module Filehub.Template.Mobile where


import Data.Aeson ((.=))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as Maybe
import Data.String.Interpolate (iii, i)
import Data.Text (Text)
import Data.Text qualified as Text
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
import Filehub.Template.Internal (viewId, toClientPath, toHxVals, sideBarId, controlPanelId, toolBarId, tableId, searchBar)
import Filehub.Template.Internal qualified as Template

import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Servant (ToHttpApiData(..))
import System.FilePath (takeFileName)
import Text.Fuzzy (simpleFilter)


index :: Bool
      -> Html ()
      -> Html ()
      -> ControlPanelState
      -> Html ()
index readOnly sideBar' view' controlPanelState = do
  div_ [ id_ "index" ] do
    overlay
    sideBar'
    view'
    controlPanel readOnly controlPanelState
    controlPanelBtn


overlay :: Html ()
overlay = div_ [ id_ overlayId ] mempty


sideBar :: [Target] -> TargetView -> Html ()
sideBar targets (TargetView currentTarget _ _) = do
  div_ [ id_ sideBarId ] do
    traverse_ targetIcon targets

  where
    targetIcon :: Target -> Html ()
    targetIcon target = do
      div_ [ class_ "target-icon"
           , term "hx-get" "/target/change"
           , term "hx-vals" $ [ "target" .= toUrlPiece (Target.getTargetId target)] & toHxVals
           , term "hx-target" "#index"
           , term "hx-swap" "outerHTML"
           ] do
        case target of
          S3Target (S3Target_ { bucket }) -> do
            i_ [ class_ "bx bxs-cube" ] mempty
            span_  [iii| /#{bucket} |]
          FileTarget (FileTarget_ { root }) -> do
            i_ [ class_ "bx bx-folder" ] mempty
            span_ [iii| /#{takeFileName root} |]
      `with` targetAttr target
      where
        targetAttr t = [class_ " current-target" | Target.getTargetId currentTarget == Target.getTargetId t]


controlPanelBtn :: Html ()
controlPanelBtn =
  button_ [ id_ controlPanelBtnId
          , term "_" [i|on click toggle .show on \##{overlayId} toggle .show on \##{controlPanelId}|]
          ] do
    i_ [ class_ "bx bx-plus" ] mempty


view :: Html () -> Html () -> Html ()
view table' pathBreadcrumb' = do
  div_ [ id_ viewId ] do
    toolBar pathBreadcrumb'
    table'


menuBtn :: Html ()
menuBtn =
  button_ [ id_ menuBtnId
          , term "_" [i|on click toggle .show on \##{overlayId} wait 50ms toggle .show on \##{sideBarId}|]
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bx-menu" ] mempty


toolBar :: Html () -> Html ()
toolBar pathBreadcrumb' = do
  div_ [ id_ toolBarId ] do
    div_ do
      menuBtn
      searchBar
    pathBreadcrumb'


search :: SearchWord -> Target -> FilePath -> [File] -> Selected -> SortFileBy -> Html ()
search (SearchWord searchWord) target root files selected order = do
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  table target root (sortFiles order filteredFiles) selected order


table :: Target -> FilePath -> [File] -> Selected -> SortFileBy -> Html ()
table target root files selected order = do
  table_ [ id_ tableId ] do
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
          `with` click file
      where
        attrs :: [Attribute]
        attrs = mconcat
          [ [ term "data-path" (Text.pack path) ]
          , [class_ "selected " | clientPath `Selected.elem` selected]
          , [id_ [i|tr-#{idx}|] ]
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
        displayTime = maybe mempty (formatTime defaultTimeLocale "%d %b %y") file.mtime


    fileNameElement :: File -> Html ()
    fileNameElement file = do
      span_ (icon >> name)
        `with` [ class_ "field"
               , title_ (Text.pack displayName)
               ]
      where
        name = span_ (toHtml displayName)

        icon =
          case file.content of
            Dir _ -> i_ [ class_ "bx bxs-folder "] mempty
            Content -> i_ [ class_ "bx bxs-file-blank "] mempty

        displayName =
          case target of
            S3Target _ -> file.path
            FileTarget _ -> takeFileName file.path

    click file =
      mconcat
      [ case file.content of
          Dir _ ->
            [ term "hx-get" ("/cd?dir=" <> toClientPath root file.path)
            , term "hx-target" ("#" <> viewId)
            , term "hx-swap" "outerHTML"
            ]
          Content
            | file.mimetype `isMime` "application/pdf" -> openBlank
            | file.mimetype `isMime` "video" || file.mimetype `isMime` "mp4" -> open
            | file.mimetype `isMime` "audio" || file.mimetype `isMime` "mp3" -> open
            | file.mimetype `isMime` "image" -> open
            | otherwise -> editor
      , case file.content of
          Dir _ -> [ class_ "dir " ]
          _ -> mempty
      ]
      where
        openBlank =
          -- Client path are percent encoded, but we need to use unencoded raw path here.
          let ClientPath path = ClientPath.toClientPath root file.path
           in [ term "_" [iii| on click js window.open('/serve?file=#{path}', '_blank'); end |] ]


        open =
          let ClientPath path = ClientPath.toClientPath root file.path
              imgIdx = Maybe.fromJust $ Map.lookup file resourceIdxMap -- image index always exists
           in [ term "_" [iii| on click send Open(path: '#{path}', index: #{imgIdx}) to body |] ]


        editor =
          [ term "hx-get" "/modal/editor"
          , term "hx-vals" $ [ "file" .= toClientPath root file.path ] & toHxVals
          , term "hx-target" "#index"
          , term "hx-swap" "beforeend"
          ]


    resourceIdxMap :: Map File Int
    resourceIdxMap = Map.fromList $ Viewer.takeResourceFiles files `zip` [0..]


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
             , term "hx-post" "/upload"
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
      button_ [ class_ "action-btn"
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
      button_ [ class_ "action-btn"
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
      button_ [ class_ "action-btn"
              , type_ "submit"
              , term "hx-get" "/cancel"
              , term "hx-target" "#index"
              , term "hx-swap" "outerHTML"
              ] do
        span_ [ class_ "field " ] do
          i_ [ class_ "bx bxs-message-alt-x" ] mempty
          span_ "Cancel"


------------------------------------
-- component ids
------------------------------------

menuBtnId :: Text
menuBtnId = "menu-btn"

controlPanelBtnId :: Text
controlPanelBtnId = "control-panel-btn"

overlayId :: Text
overlayId = "overlay"
