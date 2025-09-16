{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- File system storage backend.
--
-- === Cache
-- We use a simple cache aside strategy.
-- when reading data, we first try to read from the cache. if it's a miss, we then
-- perform the full read, then cache the result.
-- When updating, we first delete the cache, then write the full update.
module Filehub.Storage.File (storage) where

import Codec.Archive.Zip qualified as Zip
import Conduit (ConduitT, ResourceT)
import Conduit qualified
import Control.Monad (unless, when, forM_, join)
import Data.ByteString (ByteString)
import Data.ByteString (readFile)
import Data.ByteString.Lazy qualified as LBS
import Data.ClientPath (fromClientPath)
import Data.Generics.Labels ()
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Effectful ( Eff, Eff, runEff)
import Effectful.Error.Dynamic (throwError)
import Effectful.FileSystem
import Effectful.FileSystem.IO (withFile, IOMode (..))
import Effectful.FileSystem.IO.ByteString (hPut)
import Effectful.Log
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Session qualified as Session
import Filehub.Storage.Context qualified as Storage
import Filehub.Target.Types (Storage(..))
import Filehub.Types ( SessionId, File(..), FileContent(..), ClientPath )
import Lens.Micro.Platform ()
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import System.FilePath ( (</>) )
import System.IO.Temp qualified as Temp
import UnliftIO (MonadIO (..), tryIO, IOException, Handler (..))
import UnliftIO.Retry (recovering, limitRetries, exponentialBackoff)
import System.IO.Error (isDoesNotExistError)
import Data.ByteString.Builder qualified as Builder
import Effectful.Extended.Cache qualified as Cache
import Effectful.Extended.LockManager qualified as LockManager
import Data.Time (secondsToNominalDiffTime)
import Data.ByteString.Builder (Builder)
import Cache.Key (CacheKey)
import GHC.TypeLits (Symbol)
import Data.Kind (Type)


class CacheKeyComponent (s :: Symbol) a              where toCacheKeyComponent :: Builder
instance CacheKeyComponent "file"         File       where toCacheKeyComponent = "f:"
instance CacheKeyComponent "dir"          [File]     where toCacheKeyComponent = "d:"
instance CacheKeyComponent "file-content" ByteString where toCacheKeyComponent = "fcc:"
instance CacheKeyComponent "is-directory" Bool       where toCacheKeyComponent = "id:"


cacheKeyPrefix :: Builder
cacheKeyPrefix = "st:fs:"


createCacheKey :: forall (s :: Symbol) (a :: Type) . CacheKeyComponent s a => Builder -> CacheKey
createCacheKey identifier = Cache.mkCacheKey [cacheKeyPrefix, toCacheKeyComponent @s @a, identifier]


get :: forall es cacheType cacheName
    . (Storage.Context es, cacheType ~ File, cacheName ~ "file")
      => SessionId -> FilePath -> Eff es File
get sessionId  path = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> do
      pure cached
    Nothing -> do
      exists <- doesPathExist path
      file <- do
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
      Cache.insert cacheKey cacheTTL file
      pure file
  where
    cacheKey = createCacheKey @cacheName @cacheType (Builder.string8 path)
    cacheTTL = Just (secondsToNominalDiffTime 10)


isDirectory :: forall es cacheType cacheName
            . (Storage.Context es, cacheType ~ Bool, cacheName ~ "is-directory")
            => SessionId -> FilePath -> Eff es Bool
isDirectory _ filePath = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      pathExists <- doesPathExist filePath
      dirExists  <- doesDirectoryExist filePath
      result <- if not pathExists then pure False else pure dirExists
      Cache.insert cacheKey cacheTTL result
      pure result
  where
    cacheKey = createCacheKey @cacheName @cacheType (Builder.string8 filePath)
    cacheTTL = Just (secondsToNominalDiffTime 10)


read :: forall es cacheType cacheName
     . (Storage.Context es, cacheType ~ ByteString, cacheName ~ "file-content")
     => SessionId -> File -> Eff es ByteString
read _ file = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      bytes <- liftIO $ readFile file.path
      Cache.insert cacheKey cacheTTL bytes
      pure bytes
  where
    cacheKey = createCacheKey @cacheName @cacheType (Builder.string8 file.path)
    cacheTTL = Just (secondsToNominalDiffTime 10)


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
  Cache.delete (createCacheKey @"file"         @File       (Builder.string8 name))
  Cache.delete (createCacheKey @"file-content" @ByteString (Builder.string8 name))
  Cache.delete (createCacheKey @"is-directory" @Bool       (Builder.string8 name))
  Cache.delete (createCacheKey @"dir"          @[File]     (Builder.string8 name))


new :: Storage.Context es => SessionId -> String -> Eff es ()
new sessionId name = do
  filePath <- toFilePath sessionId name
  exists   <- doesFileExist filePath
  when exists do
    logAttention "[new] path doesn't exists:" filePath
    throwError (FilehubError FileExists "File already exists")
  withFile filePath ReadWriteMode (\_ -> pure ())
  Cache.delete (createCacheKey @"file"         @File       (Builder.string8 name))
  Cache.delete (createCacheKey @"file-content" @ByteString (Builder.string8 name))
  Cache.delete (createCacheKey @"is-directory" @Bool       (Builder.string8 name))
  Cache.delete (createCacheKey @"dir"          @[File]     (Builder.string8 name))


write :: Storage.Context es => SessionId -> FilePath -> ByteString -> Eff es ()
write sessionId name content = do
  LockManager.withLock (LockManager.mkLockKey name) do
    filePath <- toFilePath sessionId name
    withFile filePath WriteMode (\h -> hPut h content)
  Cache.delete (createCacheKey @"file"         @File       (Builder.string8 name))
  Cache.delete (createCacheKey @"file-content" @ByteString (Builder.string8 name))
  Cache.delete (createCacheKey @"is-directory" @Bool       (Builder.string8 name))
  Cache.delete (createCacheKey @"dir"          @[File]     (Builder.string8 name))


cp :: Storage.Context es => SessionId -> FilePath -> FilePath -> Eff es ()
cp sessionId src dst = do
  LockManager.withLock (LockManager.mkLockKey dst) do
    isDir <- isDirectory sessionId src
    if isDir then copyDirectoryRecursive src dst
    else join $ copyFile <$> toFilePath sessionId src <*> toFilePath sessionId dst
  Cache.delete (createCacheKey @"dir" @[File] (Builder.string8 dst))


-- | Copy all files and subdirectories from src to dst.
copyDirectoryRecursive :: Storage.Context es => FilePath -> FilePath -> Eff es ()
copyDirectoryRecursive src dst = do
  LockManager.withLock (LockManager.mkLockKey dst) do
    createDirectoryIfMissing True dst
    contents <- listDirectory src
    forM_ contents \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDir <- doesDirectoryExist srcPath
        if isDir
           then copyDirectoryRecursive srcPath dstPath
           else copyFile srcPath dstPath
  Cache.delete (createCacheKey @"dir" @[File] (Builder.string8 dst))


delete :: Storage.Context es => SessionId -> String -> Eff es ()
delete sessionId name = do
  filePath   <- toFilePath sessionId name
  fileExists <- doesFileExist filePath
  dirExists  <- doesDirectoryExist filePath
  if
     | fileExists -> withRetry (removeFile filePath)
     | dirExists  -> withRetry (removeDirectoryRecursive filePath)
     | otherwise  -> pure ()
  Cache.delete (createCacheKey @"file"         @File       (Builder.string8 name))
  Cache.delete (createCacheKey @"file-content" @ByteString (Builder.string8 name))
  Cache.delete (createCacheKey @"is-directory" @Bool       (Builder.string8 name))
  Cache.delete (createCacheKey @"dir"          @[File]     (Builder.string8 name))
  where
    withRetry action = recovering policy handlers \_ -> do
      result <- tryIO action
      case result of
        Left e | isDoesNotExistError e -> pure () -- it's already gone
        Left e -> liftIO (ioError e)
        Right _ -> pure ()
      where
        policy = exponentialBackoff 50000 <> limitRetries 3
        handlers =
          [ -- we need to retry unless the exception is cause becasue the file doesn't exist
            -- anymore.
            \_ -> Handler \(e :: IOException) -> pure $ not (isDoesNotExistError e)
          ]


ls :: forall es cacheType cacheName
    . (Storage.Context es, cacheType ~ [File], cacheName ~ "dir")
    => SessionId -> FilePath -> Eff es [File]
ls sessionId path = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      exists <- doesDirectoryExist path
      unless exists do
        logAttention "[lsDir] dir doesn't exists:" path
        throwError (FilehubError InvalidDir "Can't list, not a directory")
      files <- withCurrentDirectory path $
        listDirectory path
          >>= traverse makeAbsolute
          >>= traverse (get sessionId)
      Cache.insert cacheKey cacheTTL files
      pure files
  where
    cacheKey = createCacheKey @cacheName @cacheType (Builder.string8 path)
    cacheTTL = Just (secondsToNominalDiffTime 10)


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
