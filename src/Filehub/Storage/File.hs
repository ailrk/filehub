{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage.File (storage, initialize) where

import Codec.Archive.Zip (ZipOption(..))
import Codec.Archive.Zip qualified as Zip
import Control.Monad (unless, when, forM_)
import Conduit (ConduitT, ResourceT, sourceFile, sourceLazy)
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as Time
import Effectful ( Eff, Eff, (:>), IOE )
import Effectful.Error.Dynamic (throwError)
import Effectful.FileSystem
import Effectful.FileSystem.IO (withFile, IOMode (..))
import Effectful.FileSystem.IO.ByteString.Lazy (hPut, readFile)
import Effectful.Log
import Filehub.ClientPath (fromClientPath)
import Filehub.Env qualified as Env
import Filehub.Error (FilehubError(..))
import Filehub.Storage.Context qualified as Storage
import Filehub.Types ( SessionId, File(..), FileContent(..), ClientPath )
import Filehub.Storage.Internal (Storage(..))
import Filehub.Options ( FSTargetOption(..) )
import Filehub.Types
    ( FileTarget(..),
      TargetId(..) )
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import System.FilePath ( (</>) )
import System.Posix qualified as Posix
import Data.UUID.V4 qualified as UUID
import Data.Generics.Labels ()
import Data.String.Interpolate (i)
import UnliftIO (MonadIO (..))
import Lens.Micro.Platform ()
import Data.ByteString (ByteString)


get :: Storage.Context es => SessionId -> FilePath -> Eff es File
get sessionId  path = do
  isBrokenLink <- isPathBrokenSymLink path
  if isBrokenLink then do -- handle broken links.
    lstatus <- liftIO $ Posix.getSymbolicLinkStatus path
    pure File
      { path = path
      , size = Just 0
      , atime = Just $ epochToUTCTime (Posix.accessTime lstatus)
      , mtime = Just $ epochToUTCTime (Posix.statusChangeTime lstatus)
      , mimetype = "application/octet-stream"
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
      , mimetype = mimetype
      , content = if isDir then Dir Nothing else Content
      }


isDirectory :: Storage.Context es => SessionId -> FilePath -> Eff es Bool
isDirectory _ filePath = do
  pathExists <- doesPathExist filePath
  dirExists <- doesDirectoryExist filePath
  if not pathExists
     then pure False
     else pure dirExists


read :: Storage.Context es => SessionId -> File -> Eff es LBS.ByteString
read _ file = readFile file.path


readStream :: SessionId -> File -> Eff es (ConduitT () ByteString (ResourceT IO) ())
readStream _sessionId file = pure $ sourceFile file.path


newFolder :: Storage.Context es => SessionId -> String -> Eff es ()
newFolder sessionId name = do
  filePath <- toFilePath sessionId name
  exists <- doesFileExist filePath
  when exists do
    throwError FileExists
  createDirectoryIfMissing True filePath


new :: Storage.Context es => SessionId -> String -> Eff es ()
new sessionId name = do
  filePath <- toFilePath sessionId name
  exists <- doesFileExist filePath
  when exists do
    throwError FileExists
  withFile filePath ReadWriteMode (\_ -> pure ())


write :: Storage.Context es => SessionId -> String -> LBS.ByteString -> Eff es ()
write sessionId name content = do
  filePath <- toFilePath sessionId name
  withFile filePath ReadWriteMode (\h -> hPut h content)


delete :: Storage.Context es => SessionId -> String -> Eff es ()
delete sessionId name = do
  filePath <- toFilePath sessionId name
  fileExists <- doesFileExist filePath
  dirExists <- doesDirectoryExist filePath
  if
     | fileExists -> removeFile filePath
     | dirExists -> removeDirectoryRecursive filePath
     | otherwise -> pure ()


ls :: Storage.Context es => SessionId -> FilePath -> Eff es [File]
ls sessionId path = do
  exists <- doesDirectoryExist path
  unless exists do
    logAttention "[lsDir] dir doesn't exists:" path
    throwError InvalidDir
  withCurrentDirectory path $
    listDirectory path
      >>= traverse makeAbsolute
      >>= traverse (get sessionId)


cd :: Storage.Context es => SessionId -> FilePath -> Eff es ()
cd sessionId path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  Env.setCurrentDir sessionId path


lsCwd :: Storage.Context es => SessionId -> Eff es [File]
lsCwd sessionId = do
  path <- Env.getCurrentDir sessionId
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  ls sessionId path


upload :: Storage.Context es => SessionId -> MultipartData Mem -> Eff es ()
upload sessionId multipart = do
  forM_ multipart.files $ \file -> do
    let name = Text.unpack file.fdFileName
    let content = file.fdPayload
    write sessionId name content


download :: Storage.Context es => SessionId -> ClientPath -> Eff es (ConduitT () ByteString (ResourceT IO) ())
download sessionId clientPath = do
  root <- Env.getRoot sessionId
  let abspath = fromClientPath root clientPath
  file <- get sessionId abspath
  case file.content of
    Content -> readStream sessionId file
    Dir _ -> do
      archive <- liftIO $ Zip.addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] Zip.emptyArchive [file.path]
      pure . sourceLazy $ Zip.fromArchive archive


storage :: Storage.Context es => SessionId -> (Storage (Eff es))
storage sessionId =
  Storage
    { get = get sessionId
    , read = read sessionId
    , readStream = readStream sessionId
    , write = write sessionId
    , delete = delete sessionId
    , new = new sessionId
    , newFolder = newFolder sessionId
    , ls = ls sessionId
    , cd = cd sessionId
    , lsCwd = lsCwd sessionId
    , upload = upload sessionId
    , download = download sessionId
    , isDirectory = isDirectory sessionId
    }


initialize :: (IOE :> es, Log :> es, FileSystem :> es) => FSTargetOption -> Eff es FileTarget
initialize opt = do
  targetId <- liftIO $ TargetId <$> UUID.nextRandom
  root <- makeAbsolute opt.root
  logInfo_ [i|Initialized: #{targetId} - FS #{root}|]
  pure $ FileTarget_ targetId Nothing root


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
