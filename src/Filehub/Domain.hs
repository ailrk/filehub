{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ApplicativeDo #-}
module Filehub.Domain where

import Data.Time (UTCTime)
import Effectful.FileSystem
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful ((:>), Eff, IOE)
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.FileSystem.IO (withFile, IOMode (..))
import Effectful.FileSystem.IO.ByteString.Lazy (hPut)
import Control.Monad (unless, when)
import Text.Printf
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Filehub.Env (Env(..))
import UnliftIO (modifyIORef', readIORef)
import GHC.Generics
import Lens.Micro
import Data.Generics.Labels ()
import System.FilePath ((</>))


data FileContent
  = Content (Maybe ByteString)
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
    , content = if isDir then Dir Nothing else Content Nothing
    }


loadDirContents :: (FileSystem :> es, Error String :> es) => File -> Eff es File
loadDirContents file = do
  case file.content of
    Dir Nothing -> do
      files <- lsDir file.path
      pure $ file & #content .~ Dir (Just files)
    _ -> pure file


dirtree :: Traversal' File File
dirtree f = \case
  file@File { content = Content _ } -> f file
  file@File { content = Dir Nothing } -> f file
  file@File { content = Dir (Just files) } -> do
    let file' = f file
    let fs = traverse (dirtree f) files
    let modify f'@File { content = Dir Nothing } _ = f'
        modify f' xs = f' & #content . #_Dir ?~ xs
    modify <$> file' <*> fs



toFilePath :: (Reader Env :> es, IOE :> es, FileSystem :> es) => FilePath -> Eff es FilePath
toFilePath name = do
  currentDir <- asks @Env (.currentDir) >>= readIORef
  makeAbsolute (currentDir </> name)


newFolder :: (Reader Env :> es, FileSystem :> es, Error String :> es, IOE :> es) => String -> Eff es ()
newFolder name = do
  filePath <- toFilePath name
  exists <- doesFileExist filePath
  when exists do
    throwError @String (printf "%s already exists" filePath)
  withFile filePath ReadWriteMode (\_ -> pure ())
  createDirectoryIfMissing True filePath


newFile :: (Reader Env :> es, FileSystem :> es, Error String :> es, IOE :> es) => String -> Eff es ()
newFile name = do
  filePath <- toFilePath name
  exists <- doesFileExist filePath
  when exists do
    throwError @String (printf "%s already exists" filePath)
  withFile filePath ReadWriteMode (\_ -> pure ())


writeFile :: (Reader Env :> es, FileSystem :> es, Error String :> es, IOE :> es) => String -> LBS.ByteString -> Eff es ()
writeFile name content = do
  filePath <- toFilePath name
  exists <- doesFileExist filePath
  when exists do
    throwError @String (printf "%s already exists" filePath)
  withFile filePath ReadWriteMode (\h -> hPut h content)


lsDir :: (FileSystem :> es, Error String :> es) => FilePath -> Eff es [File]
lsDir path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError @String (printf "%s is not a valid directory" path)
  withCurrentDirectory path $
    listDirectory path
      >>= traverse makeAbsolute
      >>= traverse getFile


changeDir :: (Reader Env :> es, FileSystem :> es, Error String :> es, IOE :> es) => FilePath -> Eff es ()
changeDir path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError @String (printf "%s is not a valid directory" path)
  ref <- asks @Env (.currentDir)
  ref `modifyIORef'` const path


lsCurrentDir :: (Reader Env :> es, FileSystem :> es, Error String :> es, IOE :> es) => Eff es [File]
lsCurrentDir = do
  ref <- asks @Env (.currentDir)
  path <- readIORef ref
  exists <- doesDirectoryExist path
  unless exists do
    throwError @String (printf "%s is not a valid directory" path)
  lsDir path
