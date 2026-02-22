{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
  , mv
  , rename
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
import Control.Monad (unless, when, forM_)
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ClientPath (ClientPath, AbsPath (..))
import Data.ClientPath qualified as ClientPath
import Data.ClientPath.Effectful (validateAbsPath)
import Data.Coerce (coerce)
import Data.File (File (..), FileInfo, FileType (..), FileWithContent, FileContent (..), defaultFileWithContent)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Maybe (maybeToList)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (secondsToNominalDiffTime)
import Effectful ( Eff, Eff, runEff, (:>), IOE)
import Effectful.Concurrent.Async (mapConcurrently_, Concurrent)
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.Extended.Cache (Cache)
import Effectful.Extended.Cache qualified as Cache
import Effectful.Extended.LockManager (LockManager)
import Effectful.Extended.LockManager qualified as LockManager
import Effectful.FileSystem
import Effectful.FileSystem.IO (withFile, IOMode (..), hClose)
import Effectful.FileSystem.IO.ByteString (hPut)
import Effectful.Log
import Effectful.Temporary (withTempFile, Temporary)
import GHC.TypeLits (Symbol)
import Lens.Micro.Platform ()
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (Mem, FileData (..))
import Storage.Error (StorageError (..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp qualified as Temp
import Target.File (Target(..), FileSys)
import UnliftIO (MonadIO (..), tryIO, IOException, Handler (..), catch, throwIO)
import UnliftIO.Retry (recovering, limitRetries, exponentialBackoff)
import Debug.Trace (traceShowM)


class CacheKeyComponent (s :: Symbol) a              where toCacheKeyComponent :: Builder
instance CacheKeyComponent "file"         FileInfo   where toCacheKeyComponent = "f"
instance CacheKeyComponent "dir"          [FileInfo] where toCacheKeyComponent = "d"
instance CacheKeyComponent "file-content" ByteString where toCacheKeyComponent = "fc"


cacheKeyPrefix :: Builder
cacheKeyPrefix = "st:fs"


createCacheKey :: forall (s :: Symbol) (a :: Type) . CacheKeyComponent s a => Builder -> CacheKey a
createCacheKey identifier = Cache.mkCacheKey [cacheKeyPrefix, toCacheKeyComponent @s @a, identifier]


get
  :: forall es cacheType cacheName
  . ( FileSystem :> es
    , Cache      :> es
    , Log        :> es
    , cacheType ~ FileInfo
    , cacheName ~ "file")
  => AbsPath -> Eff es (Maybe FileInfo)
get path = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> do
      pure (Just cached)
    Nothing -> do
      exists <- coerce doesPathExist path
      if exists
         then do
           size  <- coerce getFileSize path
           mtime <- coerce getModificationTime path
           atime <- coerce getAccessTime path
           isDir <- isDirectory path
           let mimetype = defaultMimeLookup (coerce Text.pack path)
           let file = File
                 { path     = path
                 , size     = Just size
                 , mtime    = Just mtime
                 , atime    = Just atime
                 , mimetype = mimetype
                 , content  = if isDir then Dir else Regular
                 }
           Cache.insert cacheKey cacheDeps cacheTTL file
           pure (Just file)
          else pure Nothing
  where
    cacheKey  = createCacheKey @cacheName @cacheType (coerce Builder.string8 path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"dir" @[FileInfo] (Builder.string8 (coerce takeDirectory path))) ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


isDirectory
  :: forall es cacheType cacheName
  . ( FileSystem  :> es
    , Log   :> es
    , Cache       :> es
    , cacheType ~ FileInfo
    , cacheName ~ "file")
  => AbsPath -> Eff es Bool
isDirectory filePath = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just (File { content = Regular }) -> pure False
    Just (File { content = Dir })     -> pure True
    Nothing -> do
      pathExists <- coerce doesPathExist filePath
      dirExists  <- coerce doesDirectoryExist filePath
      result     <- if not pathExists then pure False else pure dirExists
      pure result
  where
    cacheKey = createCacheKey @cacheName @cacheType (coerce Builder.string8 filePath)


read
  :: forall es cacheType cacheName
  . ( IOE   :> es
    , Log   :> es
    , Cache :> es
    , cacheType ~ ByteString
    , cacheName ~ "file-content")
    => FileInfo -> Eff es ByteString
read File { path = AbsPath path } = do
  logTrace_ [i|[8sc2z] file read|]
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      bytes <- liftIO $ readFile path
      Cache.insert cacheKey cacheDeps cacheTTL bytes
      pure bytes
  where
    cacheKey  = createCacheKey @cacheName @cacheType (Builder.string8 path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"file" @FileInfo (Builder.string8 path)) ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


readStream :: FileInfo -> Eff es (ConduitT () ByteString (ResourceT IO) ())
readStream File{ path = AbsPath path } = pure $ Conduit.sourceFile path


newFolder
  :: ( FileSystem         :> es
     , Log                :> es
     , Cache              :> es
     , Error StorageError :> es)
  => AbsPath -> Eff es ()
newFolder path = do
  let dir = coerce takeDirectory path
  exists <- coerce doesFileExist path
  when exists do
    logAttention "[vd9fdz] path doesn't exists:" path
    throwError (FileExists "Folder already exists")
  createDirectoryIfMissing True (coerce path)
  Cache.delete (createCacheKey @"dir" @[FileInfo] (Builder.string8 dir))


new
  :: ( FileSystem         :> es
     , Log                :> es
     , Cache              :> es
     , Error StorageError :> es)
  => AbsPath -> Eff es ()
new path = do
  let dir = coerce takeDirectory path
  exists   <- coerce doesFileExist path
  when exists do
    logAttention "[9sc453] path doesn't exists:" path
    throwError (FileExists "File already exists")
  withFile (coerce path) ReadWriteMode (\_ -> pure ())
  Cache.delete (createCacheKey @"dir" @[FileInfo] (Builder.string8 dir))


write
  :: ( FileSystem  :> es
     , Temporary   :> es
     , Log         :> es
     , IOE         :> es
     , Cache       :> es
     , LockManager :> es)
  => FileWithContent -> Eff es ()
write File{ content, path = path } = do
  case content of
    FileContentRaw bytes -> go \_ h -> do hPut h bytes
    FileContentConduit conduit -> do
      go \p h -> do
        hClose h -- close the handle, sinkFile will create a handle for itself.
        liftIO . runResourceT . Conduit.runConduit
          $ conduit
          .| Conduit.sinkFile p
    FileContentDir _ -> pure ()
    FileContentNull -> pure ()
  where
    go performWrite = do
      LockManager.withLock (LockManager.mkLockKey path) do
        isCreatingNew <- coerce doesFileExist path
        let dir = coerce takeDirectory path
        let name = coerce takeFileName path
        withTempFile dir name \tempFile h -> do
          performWrite tempFile h
          when (not isCreatingNew) do
            coerce removeFile path `catch` \(e :: IOError) -> do
              when (not (isDoesNotExistError e)) do -- it's ok if file is not there.
                throwIO e
          renameFile tempFile (coerce path)
        Cache.delete (createCacheKey @"file" @FileInfo (coerce Builder.string8 path))
        when (not isCreatingNew) do
          Cache.delete (createCacheKey @"dir" @[FileInfo] (coerce Builder.string8 dir))


mv
  :: ( FileSystem         :> es
     , IOE                :> es
     , Log                :> es
     , Cache              :> es
     , LockManager        :> es
     , Concurrent         :> es
     , Error StorageError :> es)
  => [(AbsPath, AbsPath)] -> Eff es ()
mv [] = throwError (CopyError "Nothing to copy")
mv cpPairs = do
  flip mapConcurrently_ cpPairs \(src, dst) -> do
    isDir <- isDirectory src
    if isDir then copyDirectoryRecursive src dst
    else copyFile (coerce src) (coerce dst)
    delete src
    Cache.delete (createCacheKey @"dir" @[FileInfo] (Builder.string8 (coerce takeDirectory src)))
    Cache.delete (createCacheKey @"dir" @[FileInfo] (Builder.string8 (coerce takeDirectory dst)))


rename
  :: ( FileSystem         :> es
     , Cache              :> es
     , Log                :> es
     , LockManager        :> es
     , Error StorageError :> es)
  => AbsPath -> String -> Eff es ()
rename oldPath newName = do
  let dir     = coerce takeDirectory oldPath
      oldName = coerce takeFileName oldPath
  traceShowM "File.rename"
  traceShowM oldPath
  traceShowM newName
  traceShowM oldName
  traceShowM dir

  when (oldName == newName) do
    throwError (FileExists "Can't rename to itself")

  LockManager.withLock (LockManager.mkLockKey oldPath) do
    newPath <- validateAbsPath (dir </> newName) (InvalidPath ("<redacted>/" <> newName))
    LockManager.withLock (LockManager.mkLockKey newPath) do
      oldExists <- coerce doesFileExist oldPath

      when (not oldExists) do
        throwError (TargetError ("Can't find file <redacted>/" <> coerce oldName))

      newExists <- coerce doesFileExist newPath

      when (newExists) do
        throwError (TargetError ("File <redacted>/" <> coerce newName <> " already exists"))

      Cache.delete (createCacheKey @"file" @FileInfo (coerce Builder.string8 newPath))
      Cache.delete (createCacheKey @"file" @FileInfo (coerce Builder.string8 oldPath))

      coerce renameFile oldPath newPath


-- | Copy all files and subdirectories from src to dst.
copyDirectoryRecursive
  :: ( FileSystem         :> es
     , IOE                :> es
     , Cache              :> es
     , LockManager        :> es
     , Error StorageError :> es)
  => AbsPath -> AbsPath -> Eff es ()
copyDirectoryRecursive (AbsPath src) (AbsPath dst) = do
  LockManager.withLock (LockManager.mkLockKey dst) do
    createDirectoryIfMissing True dst
    contents <- listDirectory src
    forM_ contents \name -> do
        srcPath <- validateAbsPath (src </> name) (InvalidPath ("<redacted>/" <> name))
        dstPath <- validateAbsPath (dst </> name) (InvalidPath ("<redacted>/" <> name))
        isDir <- coerce doesDirectoryExist srcPath
        if isDir
           then copyDirectoryRecursive srcPath dstPath
           else coerce copyFile srcPath dstPath


delete
  :: ( FileSystem  :> es
     , IOE         :> es
     , Log         :> es
     , Cache       :> es)
  => AbsPath -> Eff es ()
delete path = do
  fileExists <- coerce doesFileExist path
  dirExists  <- coerce doesDirectoryExist path
  if
     | fileExists -> withRetry (removeFile (coerce path))
     | dirExists  -> withRetry (removeDirectoryRecursive (coerce path))
     | otherwise  -> pure ()
  Cache.delete (createCacheKey @"file" @FileInfo (coerce Builder.string8 path))
  where
    withRetry action = recovering policy handlers \_ -> do
      logInfo [i|[vhdkl2] Retrying delete|] path
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
    , cacheType ~ [FileInfo]
    , cacheName ~ "dir")
    => AbsPath -> Eff es [FileInfo]
ls path = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      exists <- coerce doesDirectoryExist path
      unless exists do
        logAttention "[idvdxa] dir doesn't exists:" path
        throwError (InvalidDir "Can't list, not a directory")
      (files, cacheDeps) <- withCurrentDirectory (coerce path) do
        unzip <$> do
          coerce listDirectory path
            >>= traverse makeAbsolute
            >>= traverse (get . coerce)
            >>= pure . fmap maybeToList
            >>= pure . mconcat
            >>= traverse \file -> do
              let depKey = SomeCacheKey (createCacheKey @"file" @FileInfo (coerce Builder.string8 file.path))
              pure (file, depKey)
      Cache.insert cacheKey cacheDeps cacheTTL files
      pure files
  where
    cacheKey = createCacheKey @cacheName @cacheType (coerce Builder.string8 path)
    cacheTTL = Just (secondsToNominalDiffTime 10)


lsCwd
  :: ( FileSystem         :> es
     , Log                :> es
     , Cache              :> es
     , Error StorageError :> es)
    => AbsPath -> Eff es [FileInfo]
lsCwd currentDir = do
  exists <- coerce doesDirectoryExist currentDir
  unless exists do
    logAttention "[icv8d3] dir doesn't exists:" currentDir
    throwError (InvalidDir "Not a directory")
  ls currentDir


upload
  :: ( FileSystem  :> es
     , Temporary   :> es
     , Log         :> es
     , IOE         :> es
     , Cache       :> es
     , LockManager :> es)
  => AbsPath -> FileData Mem -> Eff es ()
upload currentDir file = do
  let mimetype = Text.encodeUtf8 file.fdFileCType
  let name     = Text.unpack file.fdFileName
  let bytes    = LBS.toStrict file.fdPayload
  fullPath <- toFilePath currentDir name
  write $ defaultFileWithContent
    { path     = fullPath
    , mimetype = mimetype
    , content  = FileContentRaw bytes
    }


download
  :: ( FileSystem :> es
     , IOE        :> es
     , Log        :> es
     , Cache      :> es)
  => Target FileSys -> ClientPath -> Eff es (ConduitT () ByteString (ResourceT IO) ())
download fileSys clientPath = do

  let path =  ClientPath.fromClientPath fileSys.root clientPath
  mFile <- get path
  case mFile of
    Just file -> do
      case file.content of
        Regular -> readStream file
        Dir     -> do
          (zipPath, _) <- liftIO do
            tempDir <- Temp.getCanonicalTemporaryDirectory
            Temp.openTempFile tempDir "DXXXXXX.zip"

          Zip.createArchive zipPath do
            Zip.packDirRecur
              Zip.Zstd
              Zip.mkEntrySelector
              (coerce path)

          pure $
            Conduit.bracketP
              (pure ())
              (\_ -> runEff . runFileSystem $ removeFile zipPath)
              (\_ -> Conduit.sourceFile zipPath)
    Nothing ->
      pure undefined


--
-- | Helpers
--

toFilePath ::  FileSystem :> es => AbsPath-> FilePath -> Eff es AbsPath
toFilePath (AbsPath currentDir) name = do
  AbsPath <$> makeAbsolute (currentDir </> name)
