{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Domain where

import Effectful.FileSystem
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.FileSystem.IO (withFile, IOMode (..))
import Effectful.FileSystem.IO.ByteString.Lazy (hPut, readFile)
import Control.Monad (unless, when, forM_)
import Text.Printf
import Filehub.Env (Env(..))
import GHC.Generics
import Lens.Micro
import Data.Time (UTCTime)
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath ((</>), takeFileName)
import Web.FormUrlEncoded (FromForm (..), parseUnique)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import Prelude hiding (readFile, writeFile)
import Data.List (sortOn, (\\))
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Effectful.Concurrent.STM
import Codec.Archive.Zip qualified as Zip
import Codec.Archive.Zip (ZipOption(..))


------------------------------------
-- files related
------------------------------------


data FileContent
  = Content
  | Dir (Maybe [File])
  deriving (Show, Generic)


data File = File
  { path :: FilePath
  , atime :: UTCTime
  , mtime :: UTCTime
  , size :: Integer
  , content :: FileContent
  }
  deriving (Show, Generic)


getFile :: (FileSystem :> es, Error String :> es) => FilePath -> Eff es File
getFile path = do
  exists <- doesPathExist path
  unless exists do
    throwError @String (printf "%s is not a valid path" path)
  size <- getFileSize path
  mtime <- getModificationTime path
  atime <- getAccessTime path
  isDir <- doesDirectoryExist path
  pure File
    { path = path
    , size = size
    , mtime = mtime
    , atime = atime
    , content = if isDir then Dir Nothing else Content
    }


readFileContent :: (FileSystem :> es) => File -> Eff es LBS.ByteString
readFileContent file = readFile file.path


loadDirContents :: (FileSystem :> es, Error String :> es) => File -> Eff es File
loadDirContents file = do
  case file.content of
    Dir Nothing -> do
      files <- lsDir file.path
      pure $ file & #content .~ Dir (Just files)
    _ -> pure file


dirtree :: Traversal' File File
dirtree f = \case
  file@File { content = Content } -> f file
  file@File { content = Dir Nothing } -> f file
  file@File { content = Dir (Just files) } -> do
    let file' = f file
    let fs = traverse (dirtree f) files
    let modify f'@File { content = Dir Nothing } _ = f'
        modify f' xs = f' & #content . #_Dir ?~ xs
    modify <$> file' <*> fs


toFilePath :: (Reader Env :> es, Concurrent :> es, FileSystem :> es) => FilePath -> Eff es FilePath
toFilePath name = do
  currentDir <- asks @Env (.currentDir) >>= readTVarIO
  makeAbsolute (currentDir </> name)


newFolder :: (Reader Env :> es, Concurrent :> es, FileSystem :> es, Error String :> es) => String -> Eff es ()
newFolder name = do
  filePath <- toFilePath name
  exists <- doesFileExist filePath
  when exists do
    throwError @String (printf "%s already exists" filePath)
  createDirectoryIfMissing True filePath


newFile :: (Reader Env :> es, Concurrent :> es, FileSystem :> es, Error String :> es) => String -> Eff es ()
newFile name = do
  filePath <- toFilePath name
  exists <- doesFileExist filePath
  when exists do
    throwError @String (printf "%s already exists" filePath)
  withFile filePath ReadWriteMode (\_ -> pure ())


writeFile :: (Reader Env :> es, Concurrent :> es, FileSystem :> es) => String -> LBS.ByteString -> Eff es ()
writeFile name content = do
  filePath <- toFilePath name
  withFile filePath ReadWriteMode (\h -> hPut h content)


deleteFile :: (Reader Env :> es, Concurrent :> es, FileSystem :> es) => String -> Eff es ()
deleteFile name = do
  filePath <- toFilePath name
  exists <- doesFileExist filePath
  if not exists
     then pure ()
     else removeFile filePath


lsDir :: (FileSystem :> es, Error String :> es) => FilePath -> Eff es [File]
lsDir path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError @String (printf "%s is not a valid directory" path)
  withCurrentDirectory path $
    listDirectory path
      >>= traverse makeAbsolute
      >>= traverse getFile


changeDir :: (Reader Env :> es, Concurrent :> es, FileSystem :> es, Error String :> es) => FilePath -> Eff es ()
changeDir path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError @String (printf "%s is not a valid directory" path)
  ref <- asks @Env (.currentDir)
  atomically $ ref `modifyTVar` const path


lsCurrentDir :: (Reader Env :> es, Concurrent :> es, FileSystem :> es, Error String :> es) => Eff es [File]
lsCurrentDir = do
  ref <- asks @Env (.currentDir)
  path <- readTVarIO ref
  exists <- doesDirectoryExist path
  unless exists do
    throwError @String (printf "%s is not a valid directory" path)
  lsDir path


upload :: (Reader Env :> es, Concurrent :> es, FileSystem :> es) => MultipartData Mem -> Eff es ()
upload multipart = do
  forM_ multipart.files $ \file -> do
    let name = Text.unpack file.fdFileName
    let content = file.fdPayload
    writeFile name content


download :: (Reader Env :> es, Error String :> es, FileSystem :> es, IOE :> es) => ClientPath -> Eff es LBS.ByteString
download clientPath = do
  root <- asks @Env (.root)
  let abspath = fromClientPath root clientPath
  file <- getFile abspath
  case file.content of
    Content -> readFileContent file
    Dir _ -> do
      archive <- liftIO $ Zip.addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] Zip.emptyArchive [file.path]
      pure $ Zip.fromArchive archive



------------------------------------
-- new types
------------------------------------


newtype SearchWord = SearchWord Text deriving (Show, Eq, Generic)
instance FromForm SearchWord where fromForm f = SearchWord <$> parseUnique "search" f


newtype NewFile = NewFile Text deriving (Show, Eq, Generic)
instance FromForm NewFile where fromForm f = NewFile <$> parseUnique "new-file" f


newtype NewFolder = NewFolder Text deriving (Show, Eq, Generic)
instance FromForm NewFolder where fromForm f = NewFolder <$> parseUnique "new-folder" f


data UpdatedFile = UpdatedFile
  { clientPath :: ClientPath
  , content :: Text
  }
  deriving (Show, Eq, Generic)
instance FromForm UpdatedFile where
  fromForm f = do
    path <- parseUnique "path" f
    content <- parseUnique "content" f
    pure (UpdatedFile path content)


------------------------------------
-- table sort
------------------------------------


data SortFileBy
  = ByName
  | ByModified
  | BySize
  deriving (Show)


instance ToHttpApiData SortFileBy where
  toUrlPiece ByName = "name"
  toUrlPiece ByModified = "modified"
  toUrlPiece BySize = "size"


instance FromHttpApiData SortFileBy where
  parseUrlPiece "name" = pure ByName
  parseUrlPiece "modified" = pure ByModified
  parseUrlPiece "size" = pure BySize
  parseUrlPiece _ = Left "Unknown order"


sortFiles :: SortFileBy -> [File] -> [File]
sortFiles ByName = sortOn (takeFileName . (.path))
sortFiles ByModified = sortOn (.mtime)
sortFiles BySize = sortOn (.size)


------------------------------------
-- table sort
------------------------------------


-- | Filepath without the root part. The path is safe to show in the frontend.
newtype ClientPath = ClientPath { unClientPath :: FilePath }
  deriving (Show, Eq, Semigroup, Monoid)


instance ToHttpApiData ClientPath where
  toUrlPiece (ClientPath p) = toUrlPiece p


instance FromHttpApiData ClientPath where
  parseUrlPiece p = ClientPath <$> parseUrlPiece p


toClientPath :: FilePath -> FilePath -> ClientPath
toClientPath root path =
  let p = path \\ root
   in ClientPath $
     case p of
       '/':p' -> p'
       _ -> '/' : p


fromClientPath :: FilePath -> ClientPath -> FilePath
fromClientPath root (ClientPath cp) =
  root </>
    case cp of
      '/': cp' -> cp'
      _ -> cp
