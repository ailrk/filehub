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
import Filehub.Template.Internal (toClientPath, toHxVals)
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
    sideBar'
    view'
    controlPanel readOnly controlPanelState
    controlPanelBtn


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
            span_  [iii| [S3] #{bucket} |]
          FileTarget (FileTarget_ { root }) -> do
            i_ [ class_ "bx bx-folder" ] mempty
            span_ [iii| [FileSystem] #{takeFileName root} |]
      `with` targetAttr target
      where
        targetAttr t = [class_ " current-target" | Target.getTargetId currentTarget == Target.getTargetId t]


controlPanelBtn :: Html ()
controlPanelBtn =
  button_ [ class_ "btn btn-control conotrol-panel-btn"
          ] do
    span_ [ class_ "field " ] do
      i_ [ class_ "bx bxs-paste" ] mempty
      span_ "+"



controlPanel :: Bool -> ControlPanelState -> Html ()
controlPanel True _ = do
    div_ [ id_ controlPanelId ] do
      span_ [ class_ "btn-like field " ] do
        i_ [ class_ "bx bx-lock-alt" ] mempty
        span_ "Read-only is enabled"
controlPanel False state = do
  case state of
    ControlPanelDefault ->
      div_ [ id_ controlPanelId ] do
        newFolderBtn
        newFileBtn
        uploadBtn
    ControlPanelSelecting ->
      div_ [ id_ controlPanelId ] do
        newFolderBtn
        newFileBtn
        uploadBtn
        copyBtn
        deleteBtn
        cancelBtn
    ControlPanelCopied ->
      div_ [ id_ controlPanelId ] do
        newFolderBtn
        newFileBtn
        uploadBtn
        pasteBtn
        cancelBtn


view :: Html () -> Html ()
view table' = do
  div_ [ id_ viewId ] do
    toolBar
    table'


toolBar :: Html ()
toolBar = do
  div_ [ id_ "tool-bar" ] do
    searchBar


search :: SearchWord -> Target -> FilePath -> [File] -> Selected -> SortFileBy -> Html ()
search (SearchWord searchWord) target root files selected order = do
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  table target root (sortFiles order filteredFiles) selected order


searchBar :: Html ()
searchBar = do
  div_ [ id_ searchBarId ] do
    input_ [ class_ "form-control "
           , type_ "input"
           , name_ "search"
           , placeholder_ "Search as you type"
           , term "hx-post" "/search"
           , term "hx-trigger" "input changed delay:200ms, search"
           , term "hx-target" "#table"
           , term "hx-swap" "outerHTML"
           ]


table :: Target -> FilePath -> [File] -> Selected -> SortFileBy -> Html ()
table target root files selected order = do
  table_ [ id_ tableId ] do
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
        name =
          span_ (toHtml displayName) `with`
            mconcat
              [ case file.content of
                  Dir _ ->
                    [ term "hx-get" ("/cd?dir=" <> toClientPath root file.path)
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
       in [ term "_" [iii| on click js window.open('#{path}', '_blank'); end |] ]


    open file =
      let ClientPath path = ClientPath.toClientPath root file.path
          imgIdx = Maybe.fromJust $ Map.lookup file resourceIdxMap -- image index always exists
       in [ term "_" [iii| on click send Open(path: '#{path}', index: #{imgIdx}) to body |] ]


    editor file =
      [ term "hx-get" "/modal/editor"
      , term "hx-vals" $ [ "file" .= toClientPath root file.path ] & toHxVals
      , term "hx-target" "#index"
      , term "hx-swap" "beforeend"
      ]


    resourceIdxMap :: Map File Int
    resourceIdxMap = Map.fromList $ Viewer.takeResourceFiles files `zip` [0..]



------------------------------------
-- buttons
------------------------------------

newFolderBtn :: Html ()
newFolderBtn =
  button_ [ class_ "btn btn-control "
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
  button_ [ class_ "btn btn-control "
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
-- component ids
------------------------------------

viewId :: Text
viewId = "view"

controlPanelId :: Text
controlPanelId = "control-panel"

sideBarId :: Text
sideBarId = "side-bar"

searchBarId :: Text
searchBarId = "search-bar"

tableId :: Text
tableId = "table"
