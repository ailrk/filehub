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
module Storage.File
  ( get
  , isDirectory
  , read
  , readStream
  , newFolder
  , new
  , write
  , writeStream
  , cp
  , delete
  , ls
  , lsCwd
  , upload
  , download
  )
  where

import Cache.Key (CacheKey, SomeCacheKey (..))
import Codec.Archive.Zip qualified as Zip
import Conduit (ConduitT, ResourceT, (.|), runResourceT)
import Conduit qualified
import Control.Monad (unless, when, forM_, join)
import Data.ByteString (ByteString)
import Data.ByteString (readFile)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.File (File (..), FileContent (..))
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Time (secondsToNominalDiffTime)
import Effectful ( Eff, Eff, runEff, (:>), IOE)
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.Extended.Cache qualified as Cache
import Effectful.Extended.LockManager qualified as LockManager
import Effectful.FileSystem
import Effectful.FileSystem.IO (withFile, IOMode (..), hClose)
import Effectful.FileSystem.IO.ByteString (hPut)
import Effectful.Log
import GHC.TypeLits (Symbol)
import Lens.Micro.Platform ()
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp qualified as Temp
import UnliftIO (MonadIO (..), tryIO, IOException, Handler (..), catch, throwIO, Handle)
import UnliftIO.Retry (recovering, limitRetries, exponentialBackoff)
import Effectful.Extended.Cache (Cache)
import Effectful.Extended.LockManager (LockManager)
import Storage.Error (StorageError (..))
import Effectful.Temporary (withTempFile, Temporary)
import Data.String.Interpolate (i)


class CacheKeyComponent (s :: Symbol) a              where toCacheKeyComponent :: Builder
instance CacheKeyComponent "file"         File       where toCacheKeyComponent = "f"
instance CacheKeyComponent "dir"          [File]     where toCacheKeyComponent = "d"
instance CacheKeyComponent "file-content" ByteString where toCacheKeyComponent = "fc"


cacheKeyPrefix :: Builder
cacheKeyPrefix = "st:fs"


createCacheKey :: forall (s :: Symbol) (a :: Type) . CacheKeyComponent s a => Builder -> CacheKey a
createCacheKey identifier = Cache.mkCacheKey [cacheKeyPrefix, toCacheKeyComponent @s @a, identifier]


get
  :: forall es cacheType cacheName
  . ( FileSystem  :> es
    , Cache       :> es
    , cacheType ~ File
    , cacheName ~ "file")
  => FilePath -> Eff es File
get path = do
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
             isDir <- isDirectory path
             let mimetype = defaultMimeLookup (Text.pack path)
             pure File
               { path     = path
               , size     = Just size
               , mtime    = Just mtime
               , atime    = Just atime
               , mimetype = mimetype
               , content  = if isDir then Dir else Content
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
      Cache.insert cacheKey cacheDeps cacheTTL file
      pure file
  where
    cacheKey  = createCacheKey @cacheName @cacheType (Builder.string8 path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"dir" @[File] (Builder.string8 (takeDirectory path))) ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


isDirectory
  :: forall es cacheType cacheName
  . ( FileSystem  :> es
    , Cache       :> es
    , cacheType ~ File
    , cacheName ~ "file")
  => FilePath -> Eff es Bool
isDirectory filePath = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just (File { content = Content }) -> pure False
    Just (File { content = Dir })     -> pure True
    Nothing -> do
      pathExists <- doesPathExist filePath
      dirExists  <- doesDirectoryExist filePath
      result     <- if not pathExists then pure False else pure dirExists
      pure result
  where
    cacheKey = createCacheKey @cacheName @cacheType (Builder.string8 filePath)


read
  :: forall es cacheType cacheName
  . ( IOE   :> es
    , Log   :> es
    , Cache :> es
    , cacheType ~ ByteString
    , cacheName ~ "file-content")
    => File -> Eff es ByteString
read file = do
  logTrace_ [i|file read|]
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      bytes <- liftIO $ readFile file.path
      Cache.insert cacheKey cacheDeps cacheTTL bytes
      pure bytes
  where
    cacheKey  = createCacheKey @cacheName @cacheType (Builder.string8 file.path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"file" @File (Builder.string8 file.path)) ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


readStream :: File -> Eff es (ConduitT () ByteString (ResourceT IO) ())
readStream file = pure $ Conduit.sourceFile file.path


newFolder
  :: ( FileSystem         :> es
     , Log                :> es
     , Cache              :> es
     , Error StorageError :> es)
  => FilePath -> String -> Eff es ()
newFolder currentDir name = do
  filePath <- toFilePath currentDir name
  exists   <- doesFileExist filePath
  when exists do
    logAttention "[newFolder] path doesn't exists:" filePath
    throwError (FileExists "Folder already exists")
  createDirectoryIfMissing True filePath
  Cache.delete (createCacheKey @"dir" @[File] (Builder.string8 currentDir))


new
  :: ( FileSystem         :> es
     , Log                :> es
     , Cache              :> es
     , Error StorageError :> es)
  => FilePath -> String -> Eff es ()
new currentDir name = do
  filePath <- toFilePath currentDir name
  exists   <- doesFileExist filePath
  when exists do
    logAttention "[new] path doesn't exists:" filePath
    throwError (FileExists "File already exists")
  withFile filePath ReadWriteMode (\_ -> pure ())
  Cache.delete (createCacheKey @"dir" @[File] (Builder.string8 currentDir))


write
  :: ( FileSystem  :> es
     , Temporary   :> es
     , IOE         :> es
     , Cache       :> es
     , LockManager :> es)
  => FilePath -> FilePath -> ByteString -> Eff es ()
write currentDir name content = do
  write' currentDir name \_ h -> do hPut h content


writeStream
  :: ( FileSystem  :> es
     , Temporary   :> es
     , IOE         :> es
     , Cache       :> es
     , LockManager :> es)
  => FilePath -> FilePath -> ConduitT () ByteString (ResourceT IO) () -> Eff es ()
writeStream currentDir name conduit = do
  write' currentDir name \path h -> do
    hClose h -- close the handle, sinkFile will create a handle for itself.
    liftIO . runResourceT $ Conduit.runConduit (conduit .| Conduit.sinkFile path)


write'
  :: ( FileSystem  :> es
     , Temporary   :> es
     , IOE         :> es
     , Cache       :> es
     , LockManager :> es)
  => FilePath -> FilePath -> (FilePath -> Handle -> Eff es ()) -> Eff es ()
write' currentDir name performWrite = do
  LockManager.withLock (LockManager.mkLockKey name) do
    filePath      <- toFilePath currentDir name
    isCreatingNew <- doesFileExist filePath
    withTempFile currentDir (takeFileName name) \tempFile h -> do
      performWrite tempFile h
      when (not isCreatingNew) do
        removeFile filePath `catch` \(e :: IOError) -> do
          when (not (isDoesNotExistError e)) do -- it's ok if file is not there.
            throwIO e
      renameFile tempFile filePath
    Cache.delete (createCacheKey @"file" @File (Builder.string8 name))
    when (not isCreatingNew) do
      Cache.delete (createCacheKey @"dir" @[File] (Builder.string8 currentDir))


cp
  :: ( FileSystem         :> es
     , IOE                :> es
     , Cache              :> es
     , LockManager        :> es
     , Error StorageError :> es)
  => FilePath -> FilePath -> FilePath -> Eff es ()
cp currentDir src dst = do
  LockManager.withLock (LockManager.mkLockKey dst) do
    isDir <- isDirectory src
    if isDir then copyDirectoryRecursive src dst
    else join $ copyFile <$> toFilePath currentDir src <*> toFilePath currentDir dst
  Cache.delete (createCacheKey @"dir" @[File] (Builder.string8 dst))


-- | Copy all files and subdirectories from src to dst.
copyDirectoryRecursive
  :: ( FileSystem         :> es
     , IOE                :> es
     , Cache              :> es
     , LockManager        :> es
     , Error StorageError :> es)
  => FilePath -> FilePath -> Eff es ()
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


delete
  :: ( FileSystem  :> es
     , IOE         :> es
     , Log         :> es
     , Cache       :> es)
  => FilePath -> String -> Eff es ()
delete currentDir name = do
  filePath   <- toFilePath currentDir name
  fileExists <- doesFileExist filePath
  dirExists  <- doesDirectoryExist filePath
  if
     | fileExists -> withRetry (removeFile filePath)
     | dirExists  -> withRetry (removeDirectoryRecursive filePath)
     | otherwise  -> pure ()
  Cache.delete (createCacheKey @"file" @File (Builder.string8 name))
  where
    withRetry action = recovering policy handlers \_ -> do
      logInfo_ [i|Retrying delete #{name}|]
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


ls
  :: forall es cacheType cacheName
  . ( FileSystem         :> es
    , Log                :> es
    , Cache              :> es
    , Error StorageError :> es
    , cacheType ~ [File]
    , cacheName ~ "dir")
    => FilePath -> Eff es [File]
ls path = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      exists <- doesDirectoryExist path
      unless exists do
        logAttention "[lsDir] dir doesn't exists:" path
        throwError (InvalidDir "Can't list, not a directory")
      (files, cacheDeps) <- withCurrentDirectory path do
        unzip <$> do
          listDirectory path
            >>= traverse makeAbsolute
            >>= traverse \x -> do
              file <- get x
              let depKey = SomeCacheKey (createCacheKey @"file" @File (Builder.string8 x))
              pure (file, depKey)
      Cache.insert cacheKey cacheDeps cacheTTL files
      pure files
  where
    cacheKey = createCacheKey @cacheName @cacheType (Builder.string8 path)
    cacheTTL = Just (secondsToNominalDiffTime 10)


lsCwd
  :: ( FileSystem         :> es
     , Log                :> es
     , Cache              :> es
     , Error StorageError :> es)
    => FilePath -> Eff es [File]
lsCwd currentDir = do
  exists <- doesDirectoryExist currentDir
  unless exists do
    logAttention "[lsCwd] dir doesn't exists:" currentDir
    throwError (InvalidDir "Not a directory")
  ls currentDir


upload
  :: ( FileSystem  :> es
     , Temporary   :> es
     , IOE         :> es
     , Cache       :> es
     , LockManager :> es)
  => FilePath -> MultipartData Mem -> Eff es ()
upload currentDir multipart = do
  forM_ multipart.files \file -> do
    let name    = Text.unpack file.fdFileName
    let content = LBS.toStrict file.fdPayload
    write currentDir name content


download
  :: ( FileSystem :> es
     , IOE        :> es
     , Cache      :> es)
  => FilePath -> Eff es (ConduitT () ByteString (ResourceT IO) ())
download path = do
  file <- get path
  case file.content of
    Content -> readStream file
    Dir     -> do
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


--
-- | Helpers
--

toFilePath ::  FileSystem :> es => FilePath -> FilePath -> Eff es FilePath
toFilePath currentDir name = do
  makeAbsolute (currentDir </> name)
