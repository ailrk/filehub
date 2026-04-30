{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
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

import Cache.Key (CacheKey, SomeCacheKey (..))
import Codec.Archive.Zip qualified as Zip
import Conduit (ConduitT, ResourceT, (.|), runResourceT)
import Conduit qualified
import Control.Monad (unless)
import Control.Monad (when, forM_)
import Control.Service.Cache (MonadCache(..))
import Control.Service.Cache qualified as Cache
import Control.Service.LockManager qualified as LockManager
import Data.ByteString (ByteString, readFile)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ClientPath (AbsPath(..))
import Data.ClientPath (ClientPath)
import Data.ClientPath qualified as ClientPath
import Data.ClientPath.IO (validateAbsPath)
import Data.Coerce (coerce)
import Data.Conduit.Binary qualified as Conduit
import Data.File (File (..), FileInfo, FileType (..), FileWithContent, FileContent (..), defaultFileWithContent)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List (sort)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (secondsToNominalDiffTime)
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Monad (Filehub)
import Filehub.Session qualified as Session
import Filehub.Session.Types (TargetView(..))
import Filehub.Types (SessionId)
import GHC.TypeLits (Symbol)
import Lens.Micro.Platform ()
import Lens.Micro.Platform ()
import Log (logAttention, logAttention_)
import Log (logTrace_, logInfo)
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (Mem, FileData (..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp qualified as Temp
import Target.File (Target(..), FileSys)
import Target.Storage (Storage(..))
import Target.Types (handleTarget, targetHandler)
import UnliftIO (MonadIO (..), tryIO, IOException, Handler (..), catch, withFile, IOMode (..), hClose, withTempFile)
import UnliftIO (throwIO)
import UnliftIO.Async (forConcurrently_)
import UnliftIO.Directory (doesDirectoryExist)
import UnliftIO.Directory (removeFile, makeAbsolute, getFileSize, getAccessTime, getModificationTime, doesPathExist, doesFileExist, createDirectoryIfMissing, renameFile, copyFile, listDirectory, removeDirectoryRecursive, withCurrentDirectory)
import UnliftIO.Retry (recovering, limitRetries, exponentialBackoff)


cd :: SessionId -> AbsPath -> Filehub ()
cd sessionId dir = do
  exists <- doesDirectoryExist (coerce dir)
  unless exists do
    logAttention "[nmb224] dir doesn't exists:" dir
    throwIO (FilehubError InvalidDir "Can't enter, not a directory")
  Session.set sessionId (.currentDir) dir


storage :: SessionId -> Storage Filehub
storage sessionId =
  Storage
    { get         = get
    , read        = read
    , readStream  = readStream
    , ls          = ls
    , cd          = cd sessionId
    , isDirectory = isDirectory
    , write       = write
    , mv          = mv
    , rename      = rename
    , delete      = delete
    , new         = new
    , newFolder   = newFolder
    , lsCwd       = do
                      currentDir <- Session.get sessionId (.currentDir)
                      lsCwd currentDir
    , upload      = \filedata -> do
                      currentDir <- Session.get sessionId (.currentDir)
                      upload currentDir filedata
    , download    = \clientPath -> do
                      fileSys <- getFileSys sessionId
                      download fileSys clientPath
    }



getFileSys :: SessionId -> Filehub (Target FileSys)
getFileSys sessionId = do
  TargetView target _ <- Session.get sessionId (.currentTarget)
  maybe (throwIO (FilehubError TargetError "Target is not valid file system direcotry")) pure $ handleTarget target
    [ targetHandler @FileSys id
    ]




class CacheKeyComponent (s :: Symbol) a              where toCacheKeyComponent :: Builder
instance CacheKeyComponent "file"         FileInfo   where toCacheKeyComponent = "f"
instance CacheKeyComponent "dir"          [FileInfo] where toCacheKeyComponent = "d"
instance CacheKeyComponent "file-content" ByteString where toCacheKeyComponent = "fc"


cacheKeyPrefix :: Builder
cacheKeyPrefix = "st:fs"


createCacheKey :: forall (s :: Symbol) (a :: Type) . CacheKeyComponent s a => Builder -> CacheKey a
createCacheKey identifier = Cache.mkCacheKey [cacheKeyPrefix, toCacheKeyComponent @s @a, identifier]


get :: AbsPath -> Filehub FileInfo
get path = do
  mCached <- cacheLookup cacheKey
  case mCached of
    Just cached -> do
      pure cached
    Nothing -> do
      exists <- doesPathExist (coerce path)
      if exists
         then do
           size  <- getFileSize (coerce path)
           mtime <- getModificationTime (coerce path)
           atime <- getAccessTime (coerce path)
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
           cacheInsert cacheKey cacheDeps cacheTTL file
           pure file
          else do
            logAttention_ (Text.pack ("invalid path " ++ show path))
            throwIO (FilehubError InvalidPath "invalid path")
  where
    cacheKey  = createCacheKey @"file" @FileInfo (coerce Builder.string8 path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"dir" @[FileInfo] (Builder.string8 (coerce takeDirectory path))) ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


isDirectory :: AbsPath -> Filehub Bool
isDirectory filePath = do
  mCached <- cacheLookup cacheKey
  case mCached of
    Just (File { content = Regular }) -> pure False
    Just (File { content = Dir })     -> pure True
    Nothing -> do
      pathExists <- doesPathExist (coerce filePath)
      dirExists  <- doesDirectoryExist (coerce filePath)
      result     <- if not pathExists then pure False else pure dirExists
      pure result
  where
    cacheKey = createCacheKey @"file" @FileInfo (coerce Builder.string8 filePath)


read :: FileInfo -> Filehub ByteString
read File { path = AbsPath path } = do
  logTrace_ [i|[8sc2z] file read|]
  mCached <- cacheLookup cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      bytes <- liftIO $ readFile path
      cacheInsert cacheKey cacheDeps cacheTTL bytes
      pure bytes
  where
    cacheKey  = createCacheKey @"file-content" @ByteString (Builder.string8 path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"file" @FileInfo (Builder.string8 path)) ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


readStream :: FileInfo -> Maybe Integer -> Maybe Integer -> Filehub (ConduitT () ByteString (ResourceT IO) ())
readStream File{ path = AbsPath path } mOffset mCount = pure $ Conduit.sourceFileRange path mOffset mCount


newFolder :: AbsPath -> Filehub FileInfo
newFolder path = do
  let dir = coerce takeDirectory path
  exists <- doesFileExist (coerce path)
  when exists do
    logAttention "[vd9fdz] path doesn't exists:" path
    throwIO (FilehubError FileExists "Folder already exists")

  createDirectoryIfMissing True (coerce path)

  cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (Builder.string8 dir)))

  file <- get path

  pure file


new :: AbsPath -> Filehub FileInfo
new path = do
  let dir = coerce takeDirectory path
  exists   <- doesFileExist (coerce path)
  when exists do
    logAttention "[9sc453] path doesn't exists:" path
    throwIO (FilehubError FileExists "File already exists")
  withFile (coerce path) ReadWriteMode (\_ -> pure ())

  cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (Builder.string8 dir)))

  file <- get path

  pure file


write :: FileWithContent -> Filehub ()
write File{ content, path = path } = do
  case content of
    FileContentRaw bytes -> go \_ h -> do liftIO $ ByteString.hPut h bytes
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
        isCreatingNew <- doesFileExist (coerce path)
        let dir = takeDirectory (coerce path)
        let name = takeFileName (coerce path)
        withTempFile dir name \tempFile h -> do
          performWrite tempFile h
          when (not isCreatingNew) do
            removeFile (coerce path) `catch` \(e :: IOError) -> do
              when (not (isDoesNotExistError e)) do -- it's ok if file is not there.
                throwIO e
          renameFile tempFile (coerce path)
        cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo (coerce Builder.string8 path)))
        when (not isCreatingNew) do
          cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (coerce Builder.string8 dir)))


mv :: [(AbsPath, AbsPath)] -> Filehub ()
mv [] = throwIO (FilehubError CopyError "Nothing to copy")
mv cpPairs = do
  let pairs = sort cpPairs
  forConcurrently_ pairs \(src, dst) -> do
    let locks = if src == dst -- Dedup
                   then [LockManager.mkLockKey src]
                   else (fmap LockManager.mkLockKey [src, dst])
    LockManager.withLocks locks do
      isDir <- isDirectory src
      if isDir then copyDirectoryRecursive src dst
      else copyFile (coerce src) (coerce dst)
      delete' src
      cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (Builder.string8 (coerce takeDirectory src))))
      cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (Builder.string8 (coerce takeDirectory dst))))


rename :: AbsPath -> String -> Filehub ()
rename oldPath newName = do
  let dir     = coerce takeDirectory oldPath
      oldName = coerce takeFileName oldPath

  when (oldName == newName) do
    throwIO (FilehubError FileExists "Can't rename to itself")

  LockManager.withLock (LockManager.mkLockKey oldPath) do
    newPath <- validateAbsPath (dir </> newName) (FilehubError InvalidPath ("<redacted>/" <> newName))
    LockManager.withLock (LockManager.mkLockKey newPath) do
      oldExists <- doesFileExist (coerce oldPath)

      when (not oldExists) do
        throwIO (FilehubError TargetError ("Can't find file <redacted>/" <> coerce oldName))

      newExists <- doesFileExist (coerce newPath)

      when (newExists) do
        throwIO (FilehubError TargetError ("File <redacted>/" <> coerce newName <> " already exists"))

      cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo (coerce Builder.string8 newPath)))
      cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo (coerce Builder.string8 oldPath)))

      renameFile (coerce oldPath) (coerce newPath)


-- | Copy all files and subdirectories from src to dst.
copyDirectoryRecursive :: AbsPath -> AbsPath -> Filehub ()
copyDirectoryRecursive (AbsPath src) (AbsPath dst) = do
  LockManager.withLock (LockManager.mkLockKey dst) do
    createDirectoryIfMissing True dst
    contents <- listDirectory src
    forM_ contents \name -> do
        srcPath <- validateAbsPath (src </> name) (FilehubError InvalidPath ("<redacted>/" <> name))
        dstPath <- validateAbsPath (dst </> name) (FilehubError InvalidPath ("<redacted>/" <> name))
        isDir <- doesDirectoryExist (coerce srcPath)
        if isDir
           then copyDirectoryRecursive srcPath dstPath
           else copyFile (coerce srcPath) (coerce dstPath)


delete :: AbsPath -> Filehub ()
delete path = LockManager.withLock (LockManager.mkLockKey path) do delete' path


delete' :: AbsPath -> Filehub ()
delete' path = do
  fileExists <- doesFileExist (coerce path)
  dirExists  <- doesDirectoryExist (coerce path)
  if
     | fileExists -> withRetry (removeFile (coerce path))
     | dirExists  -> withRetry (removeDirectoryRecursive (coerce path))
     | otherwise  -> pure ()
  cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo (coerce Builder.string8 path)))
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


ls :: AbsPath -> Filehub [FileInfo]
ls path = do
  mCached <- cacheLookup cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      exists <- doesDirectoryExist (coerce path)
      unless exists do
        logAttention "[idvdxa] dir doesn't exists:" path
        throwIO (FilehubError InvalidDir "Can't list, not a directory")
      (files, cacheDeps) <- withCurrentDirectory (coerce path) do
        unzip <$> do
          listDirectory (coerce path)
            >>= traverse makeAbsolute
            >>= traverse (get . coerce)
            >>= traverse \file -> do
              let depKey = SomeCacheKey (createCacheKey @"file" @FileInfo (coerce Builder.string8 file.path))
              pure (file, depKey)
      cacheInsert cacheKey cacheDeps cacheTTL files
      pure files
  where
    cacheKey = createCacheKey @"dir" @[FileInfo] (coerce Builder.string8 path)
    cacheTTL = Just (secondsToNominalDiffTime 10)


lsCwd :: AbsPath -> Filehub [FileInfo]
lsCwd currentDir = do
  exists <- doesDirectoryExist (coerce currentDir)
  unless exists do
    logAttention "[icv8d3] dir doesn't exists:" currentDir
    throwIO (FilehubError InvalidDir "Not a directory")
  ls currentDir


upload :: AbsPath -> FileData Mem -> Filehub ()
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


download :: Target FileSys -> ClientPath -> Filehub (ConduitT () ByteString (ResourceT IO) ())
download fileSys clientPath = do
  let path =  ClientPath.fromClientPath fileSys.root clientPath
  file <- get path
  case file.content of
    Regular -> readStream file Nothing Nothing
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
          (\_ -> removeFile zipPath)
          (\_ -> Conduit.sourceFile zipPath)


--
-- | Helpers
--

toFilePath ::  AbsPath -> FilePath -> Filehub AbsPath
toFilePath (AbsPath currentDir) name = do
  AbsPath <$> makeAbsolute (currentDir </> name)
