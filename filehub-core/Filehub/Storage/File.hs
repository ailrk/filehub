{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
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
module Filehub.Storage.File where

import Filehub.Prelude
import Cache.Key (CacheKey, SomeCacheKey (..))
import Data.Zip qualified as Zip
import Conduit (ConduitT, ResourceT, (.|), runResourceT)
import Conduit qualified
import Control.Service.Cache (MonadCache(..))
import Control.Service.Cache qualified as Cache
import Control.Service.LockManager qualified as LockManager
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ClientPath (AbsPath(..), withinRoot)
import Data.ClientPath (ClientPath)
import Data.ClientPath qualified as ClientPath
import Data.ClientPath (validateAbsPath)
import Data.Conduit.Binary qualified as Conduit
import Data.File (File (..), FileInfo, FileType (..), FileWithContent, FileContent (..), defaultFileWithContent, IsLink (..))
import Data.Kind (Type)
import Data.List (sort)
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Monad (Filehub)
import GHC.TypeLits (Symbol)
import Log (logAttention, logAttention_)
import Log (logTrace_)
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, writeFile)
import Servant.Multipart (Mem, FileData (..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Error (isDoesNotExistError)
import System.IO (openTempFile)
import Target.File (Target(..), FileSys)
import UnliftIO (withFile, IOMode (..), hClose, withTempFile)
import UnliftIO.Directory (doesDirectoryExist, pathIsSymbolicLink, removeFile, makeAbsolute, getFileSize, getAccessTime, getModificationTime, doesPathExist, doesFileExist, createDirectoryIfMissing, renameFile, copyFile, listDirectory, removeDirectoryRecursive, withCurrentDirectory, getTemporaryDirectory, canonicalizePath)
import Control.Concurrent (threadDelay)


class CacheKeyComponent (s :: Symbol) a              where toCacheKeyComponent :: Builder
instance CacheKeyComponent "file"         FileInfo   where toCacheKeyComponent = "f"
instance CacheKeyComponent "dir"          [FileInfo] where toCacheKeyComponent = "d"
instance CacheKeyComponent "file-content" ByteString where toCacheKeyComponent = "fc"


validateFileName :: String -> Filehub ()
validateFileName name
  | null name         = throwIO (FilehubError InvalidPath "Empty filename")
  | '/' `elem` name  = throwIO (FilehubError InvalidPath "Filename may not contain '/'")
  | '\0' `elem` name = throwIO (FilehubError InvalidPath "Filename may not contain null byte")
  | name == ".."     = throwIO (FilehubError InvalidPath "Filename may not be '..'")
  | otherwise        = pure ()


cacheKeyPrefix :: Builder
cacheKeyPrefix = "st:fs"


createCacheKey :: forall s a . CacheKeyComponent s a => Builder -> CacheKey a
createCacheKey identifier = Cache.mkCacheKey [cacheKeyPrefix, toCacheKeyComponent @s @a, identifier]


getMimetype :: MonadIO m => AbsPath -> m ByteString
getMimetype path = do
  let ext = defaultMimeLookup (txt_pack (coerce path))
  if ext /= "application/octet-stream"
    then pure ext
    else do
      header <- liftIO $ tryIO $ withFile (coerce path) ReadMode (`B.hGet` 512)
      pure $ case header of
        Left  _  -> ext
        Right bs -> sniffMimetype bs `orElse` ext
  where
    orElse x fallback = if x == "application/octet-stream" then fallback else x


sniffMimetype :: ByteString -> ByteString
sniffMimetype bs
  -- images
  | "\x89PNG\r\n\x1a\n"   `bs_isPrefixOf` bs                        = "image/png"
  | "\xFF\xD8\xFF"        `bs_isPrefixOf` bs                        = "image/jpeg"
  | "GIF87a"              `bs_isPrefixOf` bs                        = "image/gif"
  | "GIF89a"              `bs_isPrefixOf` bs                        = "image/gif"
  | "RIFF"                `bs_isPrefixOf` bs
      && "WEBP"           `bs_isInfixOf`  bs_take 12 bs             = "image/webp"
  | "BM"                  `bs_isPrefixOf` bs                        = "image/bmp"
  | "\x00\x00\x01\x00"    `bs_isPrefixOf` bs                        = "image/x-icon"
  | "\x00\x00\x00\x0C\x6A\x50\x20\x20" `bs_isPrefixOf` bs           = "image/jp2"
  | "II\x2A\x00"          `bs_isPrefixOf` bs                        = "image/tiff"
  | "MM\x00\x2A"          `bs_isPrefixOf` bs                        = "image/tiff"
  -- documents
  | "%PDF"                `bs_isPrefixOf` bs                        = "application/pdf"
  -- archives
  | "PK\x03\x04"          `bs_isPrefixOf` bs                        = "application/zip"
  | "\x1f\x8b"            `bs_isPrefixOf` bs                        = "application/gzip"
  | "BZh"                 `bs_isPrefixOf` bs                        = "application/x-bzip2"
  | "\xFD7zXZ\x00"        `bs_isPrefixOf` bs                        = "application/x-xz"
  | "7z\xBC\xAF\x27\x1C"  `bs_isPrefixOf` bs                        = "application/x-7z-compressed"
  | "Rar!\x1A\x07"        `bs_isPrefixOf` bs                        = "application/vnd.rar"
  -- audio
  | "ID3"                 `bs_isPrefixOf` bs                        = "audio/mpeg"
  | "\xFF\xFB"            `bs_isPrefixOf` bs                        = "audio/mpeg"
  | "\xFF\xF3"            `bs_isPrefixOf` bs                        = "audio/mpeg"
  | "\xFF\xF2"            `bs_isPrefixOf` bs                        = "audio/mpeg"
  | "OggS"                `bs_isPrefixOf` bs                        = "audio/ogg"
  | "fLaC"                `bs_isPrefixOf` bs                        = "audio/flac"
  | "RIFF"                `bs_isPrefixOf` bs                        = "audio/wav"
  -- video
  | "\x1aE\xDF\xA3"       `bs_isPrefixOf` bs                        = "video/webm"
  | "ftyp"                `bs_isInfixOf`  bs_take 12 bs             = "video/mp4"
  -- text heuristic: all bytes are printable ASCII or common whitespace
  | isLikelyText bs                                                 = "text/plain"
  | otherwise                                                       = "application/octet-stream"
  where
    isLikelyText chunk =
      let sample = bs_take 512 chunk
       in not (bs_null sample) && bs_all (\w -> w >= 9 && w <= 126 || w >= 128) sample


get :: AbsPath -> Filehub FileInfo
get path = do
  mCached <- cacheLookup cacheKey
  case mCached of
    Just cached -> do
      pure cached
    Nothing -> do
      exists <- doesPathExist (coerce path)
      isDir  <- isDirectory path
      isLink <- pathIsSymbolicLink (coerce path) `catch` \(_ :: SomeException) -> pure False
      if
        | exists -> do
            size   <- getFileSize (coerce path)
            mtime  <- getModificationTime (coerce path)
            atime  <- getAccessTime (coerce path)
            mimetype <- getMimetype path
            let file = File
                  { path     = path
                  , size     = Just size
                  , mtime    = Just mtime
                  , atime    = Just atime
                  , mimetype = mimetype
                  , isLink   = if isLink then Link else NotLink
                  , content  = if
                                  | isDir     -> Dir
                                  | otherwise -> Regular
                  }
            cacheInsert cacheKey cacheDeps cacheTTL file
            pure file

        | isLink -> do -- Broken Link
            let file = File
                  { path     = path
                  , size     = Nothing
                  , mtime    = Nothing
                  , atime    = Nothing
                  , mimetype = "inode/symlink"
                  , isLink   = BrokenLink
                  , content  = Regular
                  }
            cacheInsert cacheKey cacheDeps cacheTTL file
            pure file

        | otherwise -> do
            logAttention_ (txt_pack ("[97zcsm] get: invalid path" ++ show path))
            throwIO (FilehubError InvalidPath "get: invalid path")
  where
    cacheKey  = createCacheKey @"file" @FileInfo (coerce BB.string8 path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"dir" @[FileInfo] (BB.string8 (coerce takeDirectory path))) ]
    cacheTTL  = Just (time_fromSeconds 10)


isDirectory :: AbsPath -> Filehub Bool
isDirectory filePath = do
  mCached <- cacheLookup cacheKey
  case mCached of
    Just (File { content = Dir }) -> pure True
    Just _                        -> pure False
    Nothing -> do
      pathExists <- doesPathExist (coerce filePath)
      dirExists  <- doesDirectoryExist (coerce filePath)
      result     <- if not pathExists then pure False else pure dirExists
      pure result
  where
    cacheKey = createCacheKey @"file" @FileInfo (coerce BB.string8 filePath)


doesExist :: AbsPath -> Filehub Bool
doesExist filePath = liftIO $ doesPathExist (coerce filePath)


read :: FileInfo -> Filehub ByteString
read File { path = AbsPath path } = do
  logTrace_ [i|[8sc2z] file read|]
  mCached <- cacheLookup cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      bytes <- liftIO $ bs_readFile path
      cacheInsert cacheKey cacheDeps cacheTTL bytes
      pure bytes
  where
    cacheKey  = createCacheKey @"file-content" @ByteString (BB.string8 path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"file" @FileInfo (BB.string8 path)) ]
    cacheTTL  = Just (time_fromSeconds 10)


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

  cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (BB.string8 dir)))

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

  cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (BB.string8 dir)))

  file <- get path

  pure file


write :: FileWithContent -> Filehub ()
write File{ content, path = path } = do
  case content of
    FileContentRaw bytes -> go \_ h -> do liftIO $ B.hPut h bytes
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
        fileExists <- doesFileExist (coerce path)
        let dir = takeDirectory (coerce path)
        let name = takeFileName (coerce path)
        withTempFile dir name \tempFile h -> do
          performWrite tempFile h
          renameFile tempFile (coerce path)
        cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo (coerce BB.string8 path)))
        when (not fileExists) do
          cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (coerce BB.string8 dir)))


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
      if isDir
        then do
          copyDirectoryRecursive src dst
          delete' src
        else do
          moved <- tryIO (renameFile (coerce src) (coerce dst))
          case moved of
            Right () -> pure ()
            Left _   -> do
              copyFile (coerce src) (coerce dst)
              delete' src
      cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (BB.string8 (coerce takeDirectory src))))
      cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] (BB.string8 (coerce takeDirectory dst))))


rename :: AbsPath -> String -> Filehub ()
rename oldPath newName = do
  let dir     = coerce takeDirectory oldPath
      oldName = coerce takeFileName oldPath

  when (oldName == newName) do
    throwIO (FilehubError FileExists "Can't rename to itself")

  validateFileName newName

  LockManager.withLock (LockManager.mkLockKey oldPath) do
    newPath <- validateAbsPath (dir </> newName) (FilehubError InvalidPath ("<redacted>/" <> newName))
    LockManager.withLock (LockManager.mkLockKey newPath) do
      oldExists <- doesFileExist (coerce oldPath)

      when (not oldExists) do
        throwIO (FilehubError TargetError ("Can't find file <redacted>/" <> coerce oldName))

      newExists <- doesFileExist (coerce newPath)

      when (newExists) do
        throwIO (FilehubError TargetError ("File <redacted>/" <> coerce newName <> " already exists"))

      cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo (coerce BB.string8 newPath)))
      cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo (coerce BB.string8 oldPath)))

      renameFile (coerce oldPath) (coerce newPath)


-- | Copy all files and subdirectories from src to dst.
-- Callers are responsible for locking src and dst before calling this.
copyDirectoryRecursive :: AbsPath -> AbsPath -> Filehub ()
copyDirectoryRecursive (AbsPath src) (AbsPath dst) = do
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
  cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo (coerce BB.string8 path)))
  where
    withRetry action = go (0 :: Int)
      where
        go n = do
          result <- tryIO action
          case result of
            Right _                        -> pure ()
            Left e | isDoesNotExistError e -> pure ()
            Left _ | n < 3                -> do
              liftIO (threadDelay (50000 * 2 ^ n))
              go (n + 1)
            Left e                         -> throwIO e


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
              let depKey = SomeCacheKey (createCacheKey @"file" @FileInfo (coerce BB.string8 file.path))
              pure (file, depKey)
      cacheInsert cacheKey cacheDeps cacheTTL files
      pure files
  where
    cacheKey = createCacheKey @"dir" @[FileInfo] (coerce BB.string8 path)
    cacheTTL = Just (time_fromSeconds 10)


lsCwd :: AbsPath -> Filehub [FileInfo]
lsCwd currentDir = do
  exists <- doesDirectoryExist (coerce currentDir)
  unless exists do
    logAttention "[icv8d3] dir doesn't exists:" currentDir
    throwIO (FilehubError InvalidDir "Not a directory")
  ls currentDir


upload :: AbsPath -> FileData Mem -> Filehub ()
upload currentDir file = do
  let mimetype = txt_encode file.fdFileCType
  let name     = txt_unpack file.fdFileName
  let bytes    = lBS_toStrict file.fdPayload
  validateFileName name
  fullPath <- toFilePath currentDir name
  write $ defaultFileWithContent
    { path     = fullPath
    , mimetype = mimetype
    , content  = FileContentRaw bytes
    }


download :: Target FileSys -> ClientPath -> Filehub (ConduitT () ByteString (ResourceT IO) ())
download fileSys clientPath = do
  let path = ClientPath.fromClientPath fileSys.root clientPath
  unless (withinRoot fileSys.root path) $
    throwIO (FilehubError InvalidPath "path escapes configured root")
  file <- get path

  case file.isLink of
    BrokenLink -> throwIO (FilehubError InvalidPath "broken link")
    _          -> pure ()

  case file.content of
    Regular    -> readStream file Nothing Nothing
    Dir     -> do
      (zipPath, _) <- liftIO do
        tempDir <- getTemporaryDirectory >>= canonicalizePath
        openTempFile tempDir "DXXXXXX.zip"

      onException
        (liftIO $ Zip.packDir zipPath (coerce path))
        (removeFile zipPath)

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
