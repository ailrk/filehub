{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ConstraintKinds #-}
module Filehub.Storage.File (storage) where

import Codec.Archive.Zip qualified as Zip
import Control.Monad (unless, when, forM_, join)
import Conduit (ConduitT, ResourceT)
import Conduit qualified
import Data.ByteString (ByteString)
import Data.ByteString (readFile)
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Effectful ( Eff, Eff, runEff )
import Effectful.Error.Dynamic (throwError)
import Effectful.FileSystem
import Effectful.FileSystem.IO (withFile, IOMode (..))
import Effectful.FileSystem.IO.ByteString (hPut)
import Effectful.Log
import Filehub.ClientPath (fromClientPath)
import Filehub.Session qualified as Session
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Types ( SessionId, File(..), FileContent(..), ClientPath )
import Filehub.Target.Types (Storage(..))
import Filehub.Storage.Context qualified as Storage
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import System.FilePath ( (</>) )
import Data.Generics.Labels ()
import UnliftIO (MonadIO (..))
import Lens.Micro.Platform ()
import System.IO.Temp qualified as Temp


get :: Storage.Context es => SessionId -> FilePath -> Eff es File
get sessionId  path = do
  exists <- doesPathExist path
  if exists
     then do
       size  <- getFileSize path
       mtime <- getModificationTime path
       atime <- getAccessTime path
       isDir <- isDirectory sessionId path
       let mimetype = defaultMimeLookup (Text.pack path)
       pure File
         { path     = path
         , size     = Just size
         , mtime    = Just mtime
         , atime    = Just atime
         , mimetype = mimetype
         , content  = if isDir then Dir Nothing else Content
         }
      else do
        pure File
          { path     = path
          , size     = Just 0
          , atime    = Nothing
          , mtime    = Nothing
          , mimetype = "application/octet-stream"
          , content  = Content
          }


isDirectory :: Storage.Context es => SessionId -> FilePath -> Eff es Bool
isDirectory _ filePath = do
  pathExists <- doesPathExist filePath
  dirExists  <- doesDirectoryExist filePath
  if not pathExists
     then pure False
     else pure dirExists


read :: Storage.Context es => SessionId -> File -> Eff es ByteString
read _ file = liftIO $ readFile file.path


readStream :: SessionId -> File -> Eff es (ConduitT () ByteString (ResourceT IO) ())
readStream _sessionId file = pure $ Conduit.sourceFile file.path


newFolder :: Storage.Context es => SessionId -> String -> Eff es ()
newFolder sessionId name = do
  filePath <- toFilePath sessionId name
  exists   <- doesFileExist filePath
  when exists do
    logAttention "[newFolder] path doesn't exists:" filePath
    throwError (FilehubError FileExists "Folder already exists")
  createDirectoryIfMissing True filePath


new :: Storage.Context es => SessionId -> String -> Eff es ()
new sessionId name = do
  filePath <- toFilePath sessionId name
  exists   <- doesFileExist filePath
  when exists do
    logAttention "[new] path doesn't exists:" filePath
    throwError (FilehubError FileExists "File already exists")
  withFile filePath ReadWriteMode (\_ -> pure ())


write :: Storage.Context es => SessionId -> String -> ByteString -> Eff es ()
write sessionId name content = do
  filePath <- toFilePath sessionId name
  withFile filePath WriteMode (\h -> hPut h content)


cp :: Storage.Context es => SessionId -> FilePath -> FilePath -> Eff es ()
cp sessionId src dest = do
  isDir <- isDirectory sessionId src
  if isDir then copyDirectoryRecursive src dest
  else join $ copyFile <$> toFilePath sessionId src <*> toFilePath sessionId dest


-- | Copy all files and subdirectories from src to dst.
copyDirectoryRecursive :: Storage.Context es => FilePath -> FilePath -> Eff es ()
copyDirectoryRecursive src dst = do
  createDirectoryIfMissing True dst
  contents <- listDirectory src
  forM_ contents \name -> do
      let srcPath = src </> name
      let dstPath = dst </> name
      isDir <- doesDirectoryExist srcPath
      if isDir
         then copyDirectoryRecursive srcPath dstPath
         else copyFile srcPath dstPath


delete :: Storage.Context es => SessionId -> String -> Eff es ()
delete sessionId name = do
  filePath   <- toFilePath sessionId name
  fileExists <- doesFileExist filePath
  dirExists  <- doesDirectoryExist filePath
  if
     | fileExists -> removeFile filePath
     | dirExists  -> removeDirectoryRecursive filePath
     | otherwise  -> pure ()


ls :: Storage.Context es => SessionId -> FilePath -> Eff es [File]
ls sessionId path = do
  exists <- doesDirectoryExist path
  unless exists do
    logAttention "[lsDir] dir doesn't exists:" path
    throwError (FilehubError InvalidDir "Can't list, not a directory")
  withCurrentDirectory path $
    listDirectory path
      >>= traverse makeAbsolute
      >>= traverse (get sessionId)


cd :: Storage.Context es => SessionId -> FilePath -> Eff es ()
cd sessionId path = do
  exists <- doesDirectoryExist path
  unless exists do
    logAttention "[cd] dir doesn't exists:" path
    throwError (FilehubError InvalidDir "Can enter, not a directory")
  Session.setCurrentDir sessionId path


lsCwd :: Storage.Context es => SessionId -> Eff es [File]
lsCwd sessionId = do
  path   <- Session.getCurrentDir sessionId
  exists <- doesDirectoryExist path
  unless exists do
    logAttention "[lsCwd] dir doesn't exists:" path
    throwError (FilehubError InvalidDir "Not a directory")
  ls sessionId path


upload :: Storage.Context es => SessionId -> MultipartData Mem -> Eff es ()
upload sessionId multipart = do
  forM_ multipart.files \file -> do
    let name    = Text.unpack file.fdFileName
    let content = LBS.toStrict file.fdPayload
    write sessionId name content


download :: Storage.Context es => SessionId -> ClientPath -> Eff es (ConduitT () ByteString (ResourceT IO) ())
download sessionId clientPath = do
  root     <- Session.getRoot sessionId
  let path =  fromClientPath root clientPath
  file     <- get sessionId path
  case file.content of
    Content -> readStream sessionId file
    Dir _ -> do
      (zipPath, _) <- liftIO do
        tempDir <- Temp.getCanonicalTemporaryDirectory
        Temp.openTempFile tempDir "DXXXXXX.zip"

      Zip.createArchive zipPath do
        Zip.packDirRecur
          Zip.Zstd
          Zip.mkEntrySelector
          path

      pure $
        Conduit.bracketP
          (pure ())
          (\_ -> runEff . runFileSystem $ removeFile zipPath)
          (\_ -> Conduit.sourceFile zipPath)


storage :: Storage.Context es => SessionId -> (Storage (Eff es))
storage sessionId =
  Storage
    { get         = get sessionId
    , read        = read sessionId
    , readStream  = readStream sessionId
    , write       = write sessionId
    , cp          = cp sessionId
    , delete      = delete sessionId
    , new         = new sessionId
    , newFolder   = newFolder sessionId
    , ls          = ls sessionId
    , cd          = cd sessionId
    , lsCwd       = lsCwd sessionId
    , upload      = upload sessionId
    , download    = download sessionId
    , isDirectory = isDirectory sessionId
    }


--
-- | Helpers
--


toFilePath :: Storage.Context es => SessionId -> FilePath -> Eff es FilePath
toFilePath sessionId name = do
  currentDir <- Session.getCurrentDir sessionId
  makeAbsolute (currentDir </> name)
