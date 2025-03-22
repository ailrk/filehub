{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}

module Filehub.Domain.File
  ( getFile
  , isDirectory
  , readFileContent
  , loadDirContents
  , dirtree
  , toFilePath
  , newFolder
  , newFile
  , writeFile
  , deleteFile
  , lsDir
  , changeDir
  , lsCurrentDir
  , upload
  , download
  , sortFiles
  )
  where


import Effectful.FileSystem
import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.FileSystem.IO (withFile, IOMode (..))
import Effectful.FileSystem.IO.ByteString.Lazy (hPut, readFile)
import Control.Monad (unless, when, forM_)
import Lens.Micro
import Lens.Micro.Platform ()
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Data.List (sortOn)
import System.FilePath ( (</>), takeFileName )
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import Prelude hiding (readFile, writeFile)
import Codec.Archive.Zip qualified as Zip
import Codec.Archive.Zip (ZipOption(..))
import Network.Mime (defaultMimeLookup)
import Filehub.Domain.ClientPath (fromClientPath)
import Filehub.Types (Env, SessionId)
import Filehub.Domain.Types (File(..), FilehubError(..), FileContent(..), SortFileBy(..), ClientPath)
import Filehub.Env qualified as Env


getFile :: (FileSystem :> es, Error FilehubError :> es) => FilePath -> Eff es File
getFile path = do
  exists <- doesPathExist path
  unless exists do
    throwError InvalidPath
  size <- getFileSize path
  mtime <- getModificationTime path
  atime <- getAccessTime path
  isDir <- isDirectory path
  let mimetype = defaultMimeLookup (Text.pack path)
  pure File
    { path = path
    , size = size
    , mtime = mtime
    , atime = atime
    , mimetype = mimetype
    , content = if isDir then Dir Nothing else Content
    }


isDirectory :: (FileSystem :> es) => FilePath -> Eff es Bool
isDirectory filePath = do
  pathExists <- doesPathExist filePath
  dirExists <- doesDirectoryExist filePath
  if not pathExists
     then pure False
     else pure dirExists


readFileContent :: (FileSystem :> es) => File -> Eff es LBS.ByteString
readFileContent file = readFile file.path


loadDirContents :: (FileSystem :> es, Error FilehubError :> es) => File -> Eff es File
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


toFilePath :: (Reader Env :> es, IOE :> es, FileSystem :> es, Error FilehubError :> es) => SessionId -> FilePath -> Eff es FilePath
toFilePath sessionId name = do
  currentDir <- Env.getCurrentDir sessionId >>= maybe (throwError InvalidSession) pure
  makeAbsolute (currentDir </> name)


newFolder :: (Reader Env :> es, IOE :> es, FileSystem :> es, Error FilehubError :> es) => SessionId -> String -> Eff es ()
newFolder sessionId name = do
  filePath <- toFilePath sessionId name
  exists <- doesFileExist filePath
  when exists do
    throwError FileExists
  createDirectoryIfMissing True filePath


newFile :: (Reader Env :> es, IOE :> es, FileSystem :> es, Error FilehubError :> es) => SessionId -> String -> Eff es ()
newFile sessionId name = do
  filePath <- toFilePath sessionId name
  exists <- doesFileExist filePath
  when exists do
    throwError FileExists
  withFile filePath ReadWriteMode (\_ -> pure ())


writeFile :: (Reader Env :> es, IOE :> es, FileSystem :> es, Error FilehubError :> es) => SessionId -> String -> LBS.ByteString -> Eff es ()
writeFile sessionId name content = do
  filePath <- toFilePath sessionId name
  withFile filePath ReadWriteMode (\h -> hPut h content)


deleteFile :: (Reader Env :> es, IOE :> es, FileSystem :> es, Error FilehubError :> es) => SessionId -> String -> Eff es ()
deleteFile sessionId name = do
  filePath <- toFilePath sessionId name
  fileExists <- doesFileExist filePath
  dirExists <- doesDirectoryExist filePath
  if
     | fileExists -> removeFile filePath
     | dirExists -> removeDirectoryRecursive filePath
     | otherwise -> pure ()


lsDir :: (FileSystem :> es, Error FilehubError :> es) => FilePath -> Eff es [File]
lsDir path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  withCurrentDirectory path $
    listDirectory path
      >>= traverse makeAbsolute
      >>= traverse getFile


changeDir :: (Reader Env :> es, IOE :> es, FileSystem :> es, Error FilehubError :> es) => SessionId -> FilePath -> Eff es ()
changeDir sessionId path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  Env.setCurrentDir sessionId path


lsCurrentDir :: (Reader Env :> es, IOE :> es, FileSystem :> es, Error FilehubError :> es) => SessionId -> Eff es [File]
lsCurrentDir sessionId = do
  path <- Env.getCurrentDir sessionId >>= maybe (throwError InvalidSession) pure
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  lsDir path


upload :: (Reader Env :> es, IOE :> es, FileSystem :> es, Error FilehubError :> es) => SessionId -> MultipartData Mem -> Eff es ()
upload sessionId multipart = do
  forM_ multipart.files $ \file -> do
    let name = Text.unpack file.fdFileName
    let content = file.fdPayload
    writeFile sessionId name content


download :: (Reader Env :> es, Error FilehubError :> es, FileSystem :> es, IOE :> es) => ClientPath -> Eff es LBS.ByteString
download clientPath = do
  root <- Env.getRoot
  let abspath = fromClientPath root clientPath
  file <- getFile abspath
  case file.content of
    Content -> readFileContent file
    Dir _ -> do
      archive <- liftIO $ Zip.addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] Zip.emptyArchive [file.path]
      pure $ Zip.fromArchive archive


sortFiles :: SortFileBy -> [File] -> [File]
sortFiles ByName = sortOn (takeFileName . (.path))
sortFiles ByModified = sortOn (.mtime)
sortFiles BySize = sortOn (.size)
