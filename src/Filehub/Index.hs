{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

module Filehub.Index where

import Lucid
import Lens.Micro
import Filehub.Template qualified as Template
import Filehub.Env (Env(..))
import Filehub.Domain (File(..), FileContent (..), NewFile (..), NewFolder(..), SearchWord(..), SortFileBy (..), sortFiles)
import Filehub.Domain qualified as Domain
import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful.Error.Dynamic (Error, runErrorNoCallStack, throwError)
import Effectful.FileSystem (FileSystem)
import UnliftIO (readIORef, writeIORef)
import GHC.Generics (Generic)
import Data.String (IsString(..))
import Data.Text qualified as Text
import Data.Maybe (fromMaybe, listToMaybe)
import Servant.Multipart (Mem, MultipartForm, MultipartData(..))
import Servant.Server.Generic (AsServerT)
import Servant (Get, errBody, err500, (:-), ServerError (..), QueryParam, Post, Put, ReqBody, FormUrlEncoded)
import Servant qualified as S
import Servant.HTML.Lucid (HTML)


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
  , sortByDropdownOn :: mode :- "dropdown" S.:> "sortby" S.:> "on" S.:> Get '[HTML] (Html ())
  , sortByDropdownOff :: mode :- "dropdown" S.:> "sortby" S.:> "off" S.:> Get '[HTML] (Html ())
  , sortTable :: mode :- "table" S.:> "sort" S.:> QueryParam "by" SortFileBy S.:> Get '[HTML] (Html ())
  }
  deriving (Generic)


server
  :: ( Reader Env :> es
     , Error ServerError :> es
     , FileSystem :> es, IOE :> es )
  => Api (AsServerT (Eff es))
server = Api
  { index = index
  , cd = \path -> Domain.changeDir (fromMaybe "/" path) & withServerError >> view ByName
  , dirs = dirs
  , newFile = \(NewFile path) -> Domain.newFile (Text.unpack path) & withServerError >> index
  , newFolder = \(NewFolder path) -> Domain.newFolder (Text.unpack path) & withServerError >> index
  , upload = \multipart -> Domain.upload multipart & withServerError >> index
  , infoModal = pure Template.infoModal
  , newFileModal = pure Template.newFileModal
  , newFolderModal = pure Template.newFolderModal
  , uploadModal = pure Template.uploadModal
  , search = \searchWord -> Template.search searchWord <$> withServerError Domain.lsCurrentDir
  , sortByDropdownOn = pure Template.sortByDropdownOn
  , sortByDropdownOff = pure Template.sortByDropdownOff
  , sortTable = \order -> view (fromMaybe ByName order)
  }


withServerError :: (Error ServerError :> es) => Eff (Error String : es) b -> Eff es b
withServerError action = do
  runErrorNoCallStack @String action >>= \case
    Left err -> throwError err500 { errBody = fromString  err }
    Right res -> pure res


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


index :: (Reader Env :> es, Error ServerError :> es, FileSystem :> es, IOE :> es) => Eff es (Html ())
index = Template.index <$> view ByName <*> tree


view :: (Error ServerError :> es, FileSystem :> es, Reader Env :> es, IOE :> es) => SortFileBy -> Eff es (Html ())
view byOrder = Template.view <$> table <*> pathBreadcrumb
  where table = Template.table <$> (sortFiles byOrder <$> withServerError Domain.lsCurrentDir)


pathBreadcrumb :: (Reader Env :> es, IOE :> es) => Eff es (Html ())
pathBreadcrumb = Template.pathBreadcrumb <$>  currentDir <*> root
  where
    currentDir = asks @Env (.currentDir) >>= readIORef
    root = asks @Env (.root)


tree :: (Reader Env :> es, IOE :> es) => Eff es (Html ())
tree = Template.tree <$> (asks @Env (.rootTree) >>= readIORef)
