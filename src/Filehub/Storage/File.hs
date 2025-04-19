{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage.File (runStorageFile) where

import Effectful.Dispatch.Dynamic (interpret)
import Effectful ( Eff, Eff, MonadIO(liftIO) )
import Effectful.FileSystem
import Effectful.Error.Dynamic (throwError)
import Effectful.Log
import Effectful.FileSystem.IO (withFile, IOMode (..))
import Effectful.FileSystem.IO.ByteString.Lazy (hPut, readFile)
import Prelude hiding (readFile, writeFile)
import Control.Monad (unless, when, forM_)
import Data.Generics.Labels ()
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as Time
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import Codec.Archive.Zip qualified as Zip
import Codec.Archive.Zip (ZipOption(..))
import Network.Mime (defaultMimeLookup)
import Filehub.Domain.ClientPath (fromClientPath)
import Filehub.Types (SessionId)
import Filehub.Domain.Types (File(..), FilehubError(..), FileContent(..), ClientPath)
import Filehub.Env qualified as Env
import Filehub.Storage.Effect (Storage (..))
import Filehub.Storage.Context qualified as Storage
import System.Posix.Files qualified as Posix
import System.Posix qualified as Posix
import System.FilePath ( (</>) )


getFile :: Storage.Context es => SessionId -> FilePath -> Eff es File
getFile sessionId  path = do
  isBrokenLink <- isPathBrokenSymLink path
  if isBrokenLink then do -- handle broken links.
    lstatus <- liftIO $ Posix.getSymbolicLinkStatus path
    pure File
      { path = path
      , size = Just 0
      , atime = Just $ epochToUTCTime (Posix.accessTime lstatus)
      , mtime = Just $ epochToUTCTime (Posix.statusChangeTime lstatus)
      , mimetype = Just "application/octet-stream"
      , content = Content
      }
  else do
    exists <- doesPathExist path
    unless exists do
      logAttention "[getFile] path doesn't exists:" path
      throwError InvalidPath
    size <- getFileSize path
    mtime <- getModificationTime path
    atime <- getAccessTime path
    isDir <- isDirectory sessionId path
    let mimetype = defaultMimeLookup (Text.pack path)
    pure File
      { path = path
      , size = Just size
      , mtime = Just mtime
      , atime = Just atime
      , mimetype = Just mimetype
      , content = if isDir then Dir Nothing else Content
      }


isDirectory :: Storage.Context es => SessionId -> FilePath -> Eff es Bool
isDirectory _ filePath = do
  pathExists <- doesPathExist filePath
  dirExists <- doesDirectoryExist filePath
  if not pathExists
     then pure False
     else pure dirExists


readFileContent :: Storage.Context es => SessionId -> File -> Eff es LBS.ByteString
readFileContent _ file = readFile file.path


newFolder :: Storage.Context es => SessionId -> String -> Eff es ()
newFolder sessionId name = do
  filePath <- toFilePath sessionId name
  exists <- doesFileExist filePath
  when exists do
    throwError FileExists
  createDirectoryIfMissing True filePath


newFile :: Storage.Context es => SessionId -> String -> Eff es ()
newFile sessionId name = do
  filePath <- toFilePath sessionId name
  exists <- doesFileExist filePath
  when exists do
    throwError FileExists
  withFile filePath ReadWriteMode (\_ -> pure ())


writeFile :: Storage.Context es => SessionId -> String -> LBS.ByteString -> Eff es ()
writeFile sessionId name content = do
  filePath <- toFilePath sessionId name
  withFile filePath ReadWriteMode (\h -> hPut h content)


deleteFile :: Storage.Context es => SessionId -> String -> Eff es ()
deleteFile sessionId name = do
  filePath <- toFilePath sessionId name
  fileExists <- doesFileExist filePath
  dirExists <- doesDirectoryExist filePath
  if
     | fileExists -> removeFile filePath
     | dirExists -> removeDirectoryRecursive filePath
     | otherwise -> pure ()


lsDir :: Storage.Context es => SessionId -> FilePath -> Eff es [File]
lsDir sessionId path = do
  exists <- doesDirectoryExist path
  unless exists do
    logAttention "[lsDir] dir doesn't exists:" path
    throwError InvalidDir
  withCurrentDirectory path $
    listDirectory path
      >>= traverse makeAbsolute
      >>= traverse (getFile sessionId)


changeDir :: Storage.Context es => SessionId -> FilePath -> Eff es ()
changeDir sessionId path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  Env.setCurrentDir sessionId path


lsCurrentDir :: Storage.Context es => SessionId -> Eff es [File]
lsCurrentDir sessionId = do
  path <- Env.getCurrentDir sessionId
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  lsDir sessionId path


upload :: Storage.Context es => SessionId -> MultipartData Mem -> Eff es ()
upload sessionId multipart = do
  forM_ multipart.files $ \file -> do
    let name = Text.unpack file.fdFileName
    let content = file.fdPayload
    writeFile sessionId name content


download :: Storage.Context es => SessionId -> ClientPath -> Eff es LBS.ByteString
download sessionId clientPath = do
  root <- Env.getRoot sessionId
  let abspath = fromClientPath root clientPath
  file <- getFile sessionId abspath
  case file.content of
    Content -> readFileContent sessionId file
    Dir _ -> do
      archive <- liftIO $ Zip.addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] Zip.emptyArchive [file.path]
      pure $ Zip.fromArchive archive


runStorageFile :: Storage.Context es => SessionId -> Eff (Storage : es) a -> Eff es a
runStorageFile sessionId = interpret $ \_ -> \case
  GetFile path -> getFile sessionId path
  IsDirectory path -> isDirectory sessionId path
  ReadFileContent file -> readFileContent sessionId file
  NewFolder path -> newFolder sessionId path
  NewFile path -> newFile sessionId path
  WriteFile path bytes -> writeFile sessionId path bytes
  DeleteFile path -> deleteFile sessionId path
  LsDir path -> lsDir sessionId path
  ChangeDir path -> changeDir sessionId path
  LsCurrentDir -> lsCurrentDir sessionId
  Upload multipart -> upload sessionId multipart
  Download clientPath -> download sessionId clientPath


--
-- | Helpers
--


epochToUTCTime :: Posix.EpochTime -> UTCTime
epochToUTCTime epoch = Time.posixSecondsToUTCTime (realToFrac epoch)


isPathBrokenSymLink :: Storage.Context es => FilePath -> Eff es Bool
isPathBrokenSymLink path = do
  isSym <- pathIsSymbolicLink path
  if isSym
     then do
       realPath <- getSymbolicLinkTarget path
       not <$> doesPathExist realPath
     else
      pure False


toFilePath :: Storage.Context es => SessionId -> FilePath -> Eff es FilePath
toFilePath sessionId name = do
  currentDir <- Env.getCurrentDir sessionId
  makeAbsolute (currentDir </> name)
