{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

module Filehub.Index where

import Effectful (Eff, (:>), IOE)
import Lucid
import Filehub.Template (withDefault)
import Effectful.Reader.Dynamic (Reader, asks)
import Filehub.Env (Env(..))
import Filehub.Domain (File(..), FileContent (..))
import Filehub.Domain qualified as Domain
import Effectful.Error.Dynamic (Error, runErrorNoCallStack, throwError)
import Effectful.FileSystem (FileSystem)
import Servant (Get, errBody, err500, (:-), ServerError (..), QueryParam, Post, Put, ReqBody, FormUrlEncoded)
import Servant qualified as S
import Servant.HTML.Lucid (HTML)
import Data.String (IsString(..))
import Data.Foldable
import UnliftIO (readIORef, writeIORef)
import System.FilePath (splitPath, takeFileName)
import Servant.Server.Generic (AsServerT)
import GHC.Generics (Generic)
import Data.Sequence qualified as Seq
import Data.Bifunctor (Bifunctor(..))
import Data.Sequence (Seq(..))
import Data.Text qualified as Text
import Data.Maybe (fromMaybe, listToMaybe)
import Lens.Micro
import Data.Text (Text)
import Servant.Multipart (Mem, MultipartForm, MultipartData(..), FileData(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Fuzzy (simpleFilter)
import Web.FormUrlEncoded


data Api mode = Api
  { index :: mode :- Get '[HTML] (Html ())
  -- ^ entrance
  , cd :: mode :- "cd" S.:> QueryParam "dir" FilePath S.:> Get '[HTML] (Html ())
  -- ^ change the current directory
  , dirs :: mode :- "dirs" S.:> QueryParam "path" FilePath S.:> Get '[HTML] (Html ())
  -- ^ render the directory tree
  , newFile :: mode :- "files" S.:> "new" S.:> ReqBody '[FormUrlEncoded] NewFile S.:> Put '[HTML] (Html ())
  -- ^ create a new file
  , newFolder :: mode :- "folders" S.:> "new" S.:> ReqBody '[FormUrlEncoded] NewFolder S.:> Put '[HTML] (Html ())
  -- ^ create a new folder
  , upload :: mode :- "upload" S.:> MultipartForm Mem (MultipartData Mem) S.:> Post '[HTML] (Html ())
  -- ^ upload a new file/folder
  , infoModal :: mode :- "modal" S.:> "info" S.:> Get '[HTML] (Html ())
  , newFileModal :: mode :- "modal" S.:> "new-file" S.:> Get '[HTML] (Html ())
  , newFolderModal :: mode :- "modal" S.:> "new-folder" S.:> Get '[HTML] (Html ())
  , uploadModal :: mode :- "modal" S.:> "upload" S.:> Get '[HTML] (Html ())
  , search :: mode :- "search" S.:> ReqBody '[FormUrlEncoded] SearchWord S.:> Post '[HTML] (Html ())
  -- ^ server side search
  }
  deriving (Generic)


newtype SearchWord = SearchWord Text deriving (Show, Eq, Generic)
instance FromForm SearchWord where fromForm f = SearchWord <$> parseUnique "search" f


newtype NewFile = NewFile Text deriving (Show, Eq, Generic)
instance FromForm NewFile where fromForm f = NewFile <$> parseUnique "new-file" f


newtype NewFolder = NewFolder Text deriving (Show, Eq, Generic)
instance FromForm NewFolder where fromForm f = NewFolder <$> parseUnique "new-folder" f


server
  :: ( Reader Env :> es
     , Error ServerError :> es
     , FileSystem :> es, IOE :> es )
  => Api (AsServerT (Eff es))
server = Api
  { index = index
  , cd = cd
  , dirs = dirs
  , newFile = newFile
  , newFolder = newFolder
  , upload = upload
  , infoModal = infoModal
  , newFileModal = newFileModal
  , newFolderModal = newFolderModal
  , uploadModal = uploadModal
  , search = search
  }


withServerError :: (Error ServerError :> es) => Eff (Error String : es) b -> Eff es b
withServerError action = do
  runErrorNoCallStack @String action >>= \case
    Left err -> throwError err500 { errBody = fromString  err }
    Right res -> pure res


cd
  :: ( Reader Env :> es
     , Error ServerError :> es
     , IOE :> es
     , FileSystem :> es)
  => Maybe FilePath -> Eff es (Html ())
cd path = do
  withServerError $ Domain.changeDir (fromMaybe "/" path)
  view


dirs
  :: ( Reader Env :> es
     , Error ServerError :> es
     , IOE :> es
     , FileSystem :> es)
  => Maybe FilePath -> Eff es (Html ())
dirs Nothing = tree
dirs (Just path) = do
  ref <- asks @Env (.rootTree)
  root <- readIORef ref
  case listToMaybe (root ^.. fileL) of
    Nothing -> pure ()
    Just f@File { content = Dir Nothing } -> do
      f' <- withServerError $ Domain.loadDirContents f
      let root' = root & fileL .~ f'
      ref `writeIORef` root'
    Just f@File { content = Dir (Just _)} -> do
      let f' = f & #content .~ Dir Nothing
      let root' = root & fileL .~ f'
      ref `writeIORef` root'
    _ -> pure ()
  tree
  where
    fileL :: Traversal' File File
    fileL = Domain.dirtree . filtered (\s -> s.path == path)


newFile
  :: ( Reader Env :> es
     , Error ServerError :> es
     , IOE :> es
     , FileSystem :> es
     )
  => NewFile -> Eff es (Html ())
newFile (NewFile path) = do
  withServerError $ Domain.newFile (Text.unpack path)
  index


newFolder
  :: ( Reader Env :> es
     , Error ServerError :> es
     , IOE :> es
     , FileSystem :> es
     )
  => NewFolder -> Eff es (Html ())
newFolder (NewFolder path) = do
  withServerError $ Domain.newFolder (Text.unpack path)
  index


upload
  :: ( Reader Env :> es
     , Error ServerError :> es
     , IOE :> es
     , FileSystem :> es
     )
  => MultipartData Mem -> Eff es (Html ())
upload multipart = do
  forM_ multipart.files $ \file -> do
    let name = Text.unpack file.fdFileName
    let content = file.fdPayload
    withServerError $ Domain.writeFile name content
  index


infoModal :: Eff es (Html ())
infoModal = pure do
  modal [ id_ componentIds.newFileModal ] do
    "Storage"


search
  :: ( Error ServerError :> es
     , FileSystem :> es
     , Reader Env :> es
     , IOE :> es
     )
  => SearchWord -> Eff es (Html ())
search (SearchWord searchWord) = do
  files <- withServerError Domain.lsCurrentDir
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  pure $ table filteredFiles


------------------------------------
-- index
------------------------------------


index
  :: ( Reader Env :> es
     , Error ServerError :> es
     , FileSystem :> es
     , IOE :> es )
  => Eff es (Html ())
index = do
  controlPanel' <- controlPanel
  view' <- view
  tree' <- tree
  pure do
    withDefault do
      div_ [ class_ "filehub " ] do
        controlPanel'
        tree'
        view'


controlPanel :: Eff es (Html ())
controlPanel = do
  pure $ do
    div_ [ id_ elementId ] do
      newFolderBtn
      newFileBtn
      uploadBtn
      sortByBtn
      viewTypeBtn
      infoBtn
  where
    elementId = componentIds.controlPanel

    newFolderBtn :: Html ()
    newFolderBtn =
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-get" "/modal/new-folder"
              , term "hx-target" "body"
              , term "hx-swap" "beforeend"
              ] "New Folder"

    newFileBtn :: Html ()
    newFileBtn  =
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-get" "/modal/new-file"
              , term "hx-target" "body"
              , term "hx-swap" "beforeend"
              ] "New File"

    uploadBtn :: Html ()
    uploadBtn = do
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-get" "/modal/upload"
              , term "hx-target" "body"
              , term "hx-swap" "beforeend"
              ] "Upload"


    sortByBtn :: Html ()
    sortByBtn = button_ [class_ "btn btn-control", type_ "submit"] "Sort By"

    viewTypeBtn :: Html ()
    viewTypeBtn = button_ [class_ "btn btn-control", type_ "submit"] "view"

    infoBtn :: Html ()
    infoBtn =
      button_ [ class_ "btn btn-control"
              , type_ "submit"
              , term "hx-get" "/modal/info"
              , term "hx-target" "body"
              , term "hx-swap" "beforeend"
              ] "info"


view
  :: ( Error ServerError :> es
     , FileSystem :> es
     , Reader Env :> es
     , IOE :> es
     )
  => Eff es (Html ())
view = do
  files <- withServerError Domain.lsCurrentDir
  let table' = table files
  pathBreadcrumb' <- pathBreadcrumb
  pure do
    div_ [ id_ componentIds.view ] do
      div_ [ id_ "tool-bar" ] do
        pathBreadcrumb'
        searchBar
      table'


pathBreadcrumb
  :: ( Reader Env :> es
     , IOE :> es
     )
  => Eff es (Html ())
pathBreadcrumb = do
  currentDir <- asks @Env (.currentDir) >>= readIORef
  root <- asks @Env (.root)
  let afterRoot path = length (splitPath path) >= length (splitPath root)
      breadcrumbItems =
        currentDir
        & splitPath
        & scanl1 (++)
        & (\xs -> if null xs then ["/"] else xs)
        & filter afterRoot
        & fmap toAttrsTuple
        & Seq.fromList
        & adjustLast (addAttr " active")
        & fmap toLi
        & sequence_
  pure do
    div_ [ class_ "breadcrumb", id_ componentIds.pathBreadcrumb ] do
      ol_ breadcrumbItems
  where
    adjustLast f xs = Seq.adjust f (length xs - 1) xs

    toAttrsTuple p = (attrs, p)
      where
        attrs =
          [ term "hx-get" ("/cd?dir=" <> Text.pack p)
          , term "hx-target" ("#" <> componentIds.view)
          , term "hx-swap" "outerHTML"
          ]

    addAttr a = first (class_ a :)

    toLi :: ([Attribute], FilePath) -> Html ()
    toLi (attrs, p) = li_ attrs . toHtml . pathShow $ p

    pathShow p =
      case Seq.fromList (splitPath p) of
        "/" :<| Seq.Empty  -> "Files"
        _ :|> l ->
          case Seq.fromList l of
            xs :|> '/' -> toList xs
            xs -> toList xs
        _ -> ""


table :: [File] -> Html ()
table files = do
  table_ [ id_ componentIds.table ] do
    thead_ do
      tr_ do
        th_ [ id_ "table-name" ] "Name"
        th_ [ id_ "table-modified" ] "Modified"
        th_ [ id_ "table-size" ] "Size"
    tbody_ $ do
      traverse_
        (\file@(File { size = size, mtime = mtime }) -> do
          tr_ do
            td_
              (fileNameElement file)
            td_ (toHtml $ formatTime defaultTimeLocale "%F %R" mtime)
            td_ (toHtml . show $ size))
        files
  where
    fileNameElement :: File -> Html ()
    fileNameElement file = do
      span_ [ class_ "file-name" ] do
        icon
        span_ attrs do
          toHtml . takeFileName $ file.path
      where
        icon =
          case file.content of
            Dir _ -> i_ [ class_ "bx bxs-folder"] mempty
            Content _ -> i_ [ class_ "bx bxs-file-blank"] mempty
        cdAttrs =
          [ class_ "breadcrumb-item "
          , term "hx-get" ("/cd?dir=" <> Text.pack file.path)
          , term "hx-target" ("#" <> componentIds.view)
          , term "hx-swap" "outerHTML"
          ]
        otherAttrs =
          case file.content of
            Dir _ -> [ class_ "dir " ]
            _ -> []
        attrs = mconcat [ cdAttrs, otherAttrs ]


searchBar :: Html ()
searchBar = do
  div_ [ id_ componentIds.searchBar ] do
    input_ [ class_ "form-control "
           , type_ "search"
           , name_ "search"
           , placeholder_ "Search as you type"
           , term "hx-post" "/search"
           , term "hx-trigger" "input changed delay:200ms, search"
           , term "hx-target" "#table"
           , term "hx-swap" "outerHTML"
           , term "hx-indicator" ".htmx-indicator"
           ]

tree
  :: ( Reader Env :> es
     , IOE :> es
     )
  => Eff es (Html ())
tree = do
  root <- asks @Env (.rootTree) >>= readIORef
  pure do
    ul_ [ id_ elementId ] $ renderFile 0 root
  where
    elementId = componentIds.tree

    cdAttrs p =
      [ term "hx-get" ("/cd?dir=" <> Text.pack p)
      , term "hx-target" ("#" <> componentIds.view)
      , term "hx-swap" "outerHTML"
      ]

    treeFoldAttrs p =
      [ term "hx-get" ("/dirs?path=" <> Text.pack p)
      , term "hx-swap" "outerHTML"
      , term "hx-target" ("#" <> elementId)
      ]

    indent n = term "indent" (Text.pack . show $ n)

    renderFile :: Int -> File -> Html ()
    renderFile n file = do
      let path = file.path
      let name = toHtml . takeFileName $ path
      case file.content of
        Content _ -> do
          li_ [ class_ "dirtree-entry file-name", indent n ] name

        Dir Nothing -> do
          let icon = i_ [ class_ "bx bx-caret-right"] mempty
          li_ [ class_ "dirtree-entry dir file-name", indent n ] do
            span_ (treeFoldAttrs path) icon
            span_ (cdAttrs path) name

        Dir (Just files) -> do
          let icon = i_ [ class_ "bx bx-caret-down"] mempty
          li_ [ class_ "dirtree-entry dir file-name", indent n ] do
            span_ (treeFoldAttrs path) icon
            span_ (cdAttrs path) name
          traverse_ (renderFile (n + 1)) files


sortByDropDown :: Html ()
sortByDropDown = do
  pure ()


newFileModal :: Eff es (Html ())
newFileModal = pure do
  modal [ id_ componentIds.newFileModal ] do
    "File"
    br_ mempty >> br_ mempty
    input_ [ class_ "form-control "
           , type_ "text"
           , name_ "new-file"
           , placeholder_ "Search as you type"
           , term "hx-put" "/files/new"
           ]
    br_ mempty >> br_ mempty
    button_ [ class_ "btn btn-modal-confirm mr-2"
            , term "_" "on click trigger closeModal"
            ] "CREATE"

    button_ [ class_ "btn btn-modal-close"
            , term "_" "on click trigger closeModal"
            ] "CLOSE"


newFolderModal :: Eff es (Html ())
newFolderModal = pure do
  modal [ id_ componentIds.newFolderModal ] do
    "Folder"
    br_ mempty >> br_ mempty
    input_ [ class_ "form-control "
           , type_ "text"
           , name_ "new-folder"
           , placeholder_ "Search as you type"
           , term "hx-put" "/folders/new"
           ]
    br_ mempty >> br_ mempty
    button_ [ class_ "btn btn-modal-confirm mr-2"
            , term "_" "on click trigger closeModal"
            ] "CREATE"

    button_ [ class_ "btn btn-modal-close"
            , term "_" "on click trigger closeModal"
            ] "CLOSE"


uploadModal :: Eff es (Html ())
uploadModal = pure do
  modal [ id_ componentIds.updateModal ] do
    "Upload"
    br_ mempty >> br_ mempty
    form_ [ term "hx-encoding" "multipart/form-data"
          , term "hx-post" "/upload"
          , term "_" "on htmx:xhr:progress(loaded, total) set #progress.value to (loaded/total)*100"
          ] do
      input_ [ class_ "btn btn-control"
             , type_ "file"
             , name_ "file"
             ]

      br_ mempty >> br_ mempty

      button_ [ class_ "btn btn-modal-confirm mr-2"
              , term "_" "on click trigger closeModal"
              ] "UPLOAD"

      button_ [ class_ "btn btn-modal-close"
              , term "_" "on click trigger closeModal"
              ] "CLOSE"



modal :: [Attribute] -> Html () -> Html ()
modal attrs body = do
  div_ ([ class_ "modal ", closeModalScript ] <> attrs) do
    div_ [ class_ "modal-underlay"
         , term "_" "on click trigger closeModal"
         ] mempty
    div_ [ class_ "modal-content" ] do
      body
  where
    closeModalScript = term "_" "on closeModal add .closing then wait for animationend then remove me"


------------------------------------
-- index
------------------------------------


data ComponentIds = ComponentIds
  { tree :: Text
  , view :: Text
  , controlPanel :: Text
  , searchBar :: Text
  , pathBreadcrumb :: Text
  , table :: Text
  , newFileModal :: Text
  , newFolderModal :: Text
  , updateModal :: Text
  }
  deriving Show


componentIds :: ComponentIds
componentIds = ComponentIds
  { tree = "tree"
  , view = "view"
  , controlPanel = "control-panel"
  , searchBar = "search-bar"
  , pathBreadcrumb = "path-breadcrumb"
  , table = "table"
  , newFileModal = "new-file-modal"
  , newFolderModal = "new-folder-modal"
  , updateModal = "update-modal"
  }
