{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- S3 storage backend.
--
-- === Cache
-- We use a simple cache aside strategy.
-- when reading data, we first try to read from the cache. if it's a miss, we then
-- perform the full read, then cache the result.
-- When updating, we first delete the cache, then write the full update.
module Storage.S3
  ( get
  , isDirectory
  , read
  , readStream
  , new
  , write
  , writeStream
  , mv
  , delete
  , ls
  , lsCwd
  , upload
  , download
  )
  where

import Amazonka (send, runResourceT, toBody, ResponseBody (..))
import Amazonka.Data qualified as Amazonka
import Amazonka.S3 (Object(..), CommonPrefix, CompletedMultipartUpload (CompletedMultipartUpload'))
import Amazonka.S3 qualified as Amazonka
import Amazonka.S3.CompleteMultipartUpload (CompleteMultipartUpload(..))
import Amazonka.S3.CreateMultipartUpload (CreateMultipartUploadResponse(..))
import Amazonka.S3.Lens qualified as Amazonka
import Amazonka.S3.UploadPart (UploadPartResponse(..))
import Cache.Key (CacheKey, SomeCacheKey (..))
import Codec.Archive.Zip qualified as Zip
import Conduit (ResourceT, MonadTrans (..), sinkLazy)
import Conduit qualified
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit
import Data.File (File (..), FileContent (..))
import Data.Foldable (forM_)
import Data.Function (fix)
import Data.Generics.Labels ()
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List (uncons)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (secondsToNominalDiffTime)
import Effectful (Eff, Eff, MonadIO (..), runEff, (:>), IOE)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Extended.Cache (Cache)
import Effectful.Extended.Cache qualified as Cache
import Effectful.FileSystem (runFileSystem, removeFile)
import Effectful.Log (Log)
import GHC.TypeLits (Symbol)
import Lens.Micro
import Lens.Micro.Platform ()
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import Storage.Error (StorageError (..))
import System.IO.Temp qualified as Temp
import Target.S3 (Backend(..), S3)
import Target.Types (TargetId)
import Target.Types.TargetId qualified as TargetId
import Debug.Trace


class CacheKeyComponent (s :: Symbol) a              where toCacheKeyComponent :: Builder
instance CacheKeyComponent "file"         File       where toCacheKeyComponent = "f"
instance CacheKeyComponent "dir"          [File]     where toCacheKeyComponent = "d"
instance CacheKeyComponent "file-content" ByteString where toCacheKeyComponent = "fc"
instance CacheKeyComponent "is-directory" Bool       where toCacheKeyComponent = "id"


cacheKeyPrefix :: Builder
cacheKeyPrefix = "st:s3:"


createCacheKey :: forall (s :: Symbol) (a :: Type) . CacheKeyComponent s a => TargetId -> Builder -> CacheKey a
createCacheKey targetId identifier = Cache.mkCacheKey
  [cacheKeyPrefix, TargetId.targetIdBuilder targetId, toCacheKeyComponent @s @a, identifier]


get
  :: forall es cacheType cacheName
  . ( IOE   :> es
    , Log   :> es
    , Cache :> es
    , cacheType ~ File
    , cacheName ~ "file")
    => Backend S3 -> FilePath -> Eff es File
get (s3@S3Backend { targetId }) path = do
  mCached      <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      let bucket  = Amazonka.BucketName s3.bucket
      let key     = Amazonka.ObjectKey (Text.pack path)
      let request = Amazonka.newHeadObject bucket key
      resp <- runResourceT $ send s3.env request
      let mtime       = resp ^. Amazonka.headObjectResponse_lastModified
      let size        = resp ^. Amazonka.headObjectResponse_contentLength
      let contentType = resp ^. Amazonka.headObjectResponse_contentType
      let file = File
            { path = path
            , atime = Nothing
            , mtime = mtime
            , size = size
            , mimetype = maybe "application/octet-stream" Text.encodeUtf8 contentType
            , content = Content
            }
      Cache.insert cacheKey cacheDeps cacheTTL file
      pure file
  where
    cacheKey  =  createCacheKey @cacheName @cacheType targetId (Builder.string8 path)
    cacheDeps = [ SomeCacheKey (createCacheKey @cacheName @cacheType targetId "") ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


-- | Because S3 doesn't have real directory, we need to list all keys in the
-- bucket and check if the file path is prefix of any key.
isDirectory
  :: forall es
  . ( Cache :> es
    , Log   :> es
    , IOE   :> es )
  => Backend S3 -> FilePath -> Eff es Bool
isDirectory s3@S3Backend { targetId } filePath = do
  mCached <- Cache.lookup @File cacheKey
  case mCached of
    Just (File { content = Content }) -> pure False
    Just (File { content = Dir })     -> pure True
    Nothing -> do
      let bucket = Amazonka.BucketName s3.bucket
      let request = Amazonka.newListObjectsV2 bucket
                  & Amazonka.listObjectsV2_prefix ?~ Text.pack (normalizeDirPath filePath)
                  & Amazonka.listObjectsV2_maxKeys ?~ 1
      resp <- runResourceT $ send s3.env request
      let result = maybe False (> 0) (resp ^. Amazonka.listObjectsV2Response_keyCount)
      pure result
  where
    cacheKey = createCacheKey @"file" @File targetId (Builder.string8 filePath)


read
  :: forall es cacheType cacheName
  . ( IOE   :> es
    , Log   :> es
    , Cache :> es
    , cacheType ~ ByteString
    , cacheName ~ "file-content")
  => Backend S3 -> File -> Eff es ByteString
read s3@S3Backend { targetId }  file = do
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      stream <- readStream s3 file
      chunks <- liftIO $ runResourceT . Conduit.runConduit $ stream Conduit..| Conduit.sinkList
      let result = LBS.toStrict (LBS.fromChunks chunks)
      Cache.insert cacheKey cacheDeps cacheTTL result
      pure result
  where
    cacheKey  = createCacheKey @cacheName @cacheType targetId (Builder.string8 file.path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"file" @File targetId (Builder.string8 file.path)) ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


readStream :: Backend S3 -> File -> Eff es (ConduitT () ByteString (ResourceT IO) ())
readStream s3 file = do
  let bucket  = Amazonka.BucketName s3.bucket
      key     = Amazonka.ObjectKey (Text.pack file.path)
      request = Amazonka.newGetObject bucket key
  pure $ do
    resp <- lift $ send s3.env request
    let (ResponseBody conduit) = resp ^. Amazonka.getObjectResponse_body
    conduit


new
  :: ( Cache :> es
     , Log   :> es
     , IOE   :> es)
  => Backend S3 -> FilePath -> Eff es ()
new s3@S3Backend { targetId } filePath = do
  write s3 filePath mempty
  Cache.delete (createCacheKey @"dir" @[File] targetId "")


write
  :: ( Cache :> es
     , Log   :> es
     , IOE   :> es)
  => Backend S3 -> FilePath -> ByteString -> Eff es ()
write s3@S3Backend { targetId } filePath bytes = do
  let bucket  = Amazonka.BucketName s3.bucket
  let key     = Amazonka.ObjectKey (Text.pack filePath)
  let request = Amazonka.newPutObject bucket key (toBody bytes)
  void . runResourceT $ send s3.env request
  Cache.delete (createCacheKey @"file" @File targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"dir" @[File] targetId "")


-- | Streaming data to S3.
writeStream
  :: ( Cache :> es
     , Log   :> es
     , IOE   :> es)
  => Backend S3 -> FilePath -> ConduitT () ByteString (ResourceT IO) () -> Maybe Integer -> Eff es ()
writeStream s3@S3Backend { targetId } filePath conduit mSize = do
  case mSize of
    Nothing -> writeMultipart s3 filePath conduit
    Just size
      | size < threshold  -> writePutObject s3 filePath conduit
      | otherwise -> writeMultipart s3 filePath conduit
  Cache.delete (createCacheKey @"file" @File targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"dir" @[File] targetId "")
  where
    threshold = 5 * 1024 * 1024 -- use putObject if it's smaller than single part.


-- | Write with S3:PutObject api. Suitable for writing small files.
-- We need to load the whole file into memory to compute the checksum. AWS S3 has chunked protocol allows you to sign chunk
-- by chunk, but it's not supported by most other S3 providers.
writePutObject :: ( IOE :> es) => Backend S3 -> FilePath -> ConduitT () ByteString (ResourceT IO) () ->  Eff es ()
writePutObject s3 filePath conduit = do
  lazyBytes <- liftIO . runResourceT . runConduit $ conduit .| sinkLazy
  let bucket  = Amazonka.BucketName s3.bucket
  let key     = Amazonka.ObjectKey (Text.pack filePath)
  let request = Amazonka.newPutObject bucket key (toBody lazyBytes)
  void . runResourceT $ send s3.env request


writeMultipart :: (IOE :> es) => Backend S3 -> FilePath -> ConduitT () ByteString (ResourceT IO) () -> Eff es ()
writeMultipart s3 filePath conduit = do
  let bucket   = Amazonka.BucketName s3.bucket
  let key      = Amazonka.ObjectKey (Text.pack filePath)
  let partSize = 5 * 1024 * 1024
  createMultipartUploadResp <- runResourceT $ send s3.env (Amazonka.newCreateMultipartUpload bucket key)
  let uploadId = createMultipartUploadResp.uploadId
  completedParts <- liftIO . runResourceT . runConduit
    $ conduit
    .| chunking partSize
    .| fix (\loop -> do
        mRes <- await
        case mRes of
         Just (partNum, chunkBuilder) -> do
           let chunk = chunkBuilderToByteString chunkBuilder
           uploadPartResp <- send s3.env (Amazonka.newUploadPart bucket key partNum uploadId (toBody chunk))
           let etag = case uploadPartResp.eTag of
                        Just x -> x
                        Nothing -> error "handle later"
           let completedPart = Amazonka.newCompletedPart partNum etag
           yield completedPart
           loop
         Nothing -> pure ())
    .| Conduit.sinkList
  void . runResourceT $ send s3.env
        (Amazonka.newCompleteMultipartUpload bucket key uploadId)
          { multipartUpload = Just (CompletedMultipartUpload' (Just (NonEmpty.fromList completedParts)))
          }


data ChunkBuilder
  = ChunkBuilder Builder Int
  | ChunkBuilded ByteString


chunkBuilderToByteString :: ChunkBuilder -> ByteString
chunkBuilderToByteString = \case
  ChunkBuilder builder _ -> ByteString.toStrict . Builder.toLazyByteString $ builder
  ChunkBuilded bytes     -> bytes


chunkBuilderSize :: ChunkBuilder -> Int
chunkBuilderSize = \case
  ChunkBuilder _ size -> size
  ChunkBuilded bytes  -> ByteString.length bytes


-- | It's slow if any one parameter is ChunkBuiled.
instance Semigroup ChunkBuilder where
  ChunkBuilder b1 s1 <> ChunkBuilder b2 s2 = ChunkBuilder (b1 <> b2) (s1 + s2)
  ChunkBuilded b1 <> c2 = ChunkBuilder (Builder.byteString b1) (ByteString.length b1) <> c2
  c1 <> ChunkBuilded b2 = c1 <> ChunkBuilder (Builder.byteString b2) (ByteString.length b2)


-- Chunk a ByteString Conduit into exact n-byte pieces and track chunk count
chunking :: MonadIO m => Int -> ConduitT ByteString (Int, ChunkBuilder) m ()
chunking chunkSize = flip fix (1, ChunkBuilder (Builder.byteString ByteString.empty) 0)
  \rec (idx, acc) -> do
    mBytes <- await
    case mBytes of
      Nothing ->
        case acc of
          ChunkBuilder _ size
            | not (size == 0) -> yield (idx, acc) -- done
            | otherwise       -> pure ()
          ChunkBuilded _ -> pure ()
      Just bytes -> do
        let combined  = acc <> ChunkBuilder (Builder.byteString bytes) (ByteString.length bytes)
        let cbSize    = chunkBuilderSize combined
        traceShowM ("got bytes " <> show cbSize)
        if cbSize >= chunkSize
          then do
            let combinedBytes = chunkBuilderToByteString combined
            let (chunk, rest) = ByteString.splitAt chunkSize combinedBytes
            yield (idx, ChunkBuilded chunk)
            rec (idx + 1, ChunkBuilder (Builder.byteString rest) (cbSize - chunkSize))
          else
            rec (idx, combined)


mv
  :: ( IOE                :> es
     , Log                :> es
     , Cache              :> es
     , Error StorageError :> es)
   => Backend S3 -> [(FilePath, FilePath)] -> Eff es ()
mv _ [] = throwError (CopyError "Nothing to copy")
mv s3@S3Backend { targetId } cpPairs = do
  forM_ cpPairs \(src, dst) -> do
    let bucket  = Amazonka.BucketName s3.bucket
    let destKey = Amazonka.ObjectKey (Text.pack dst)
    let request = Amazonka.newCopyObject bucket (Text.pack src) destKey
    void . runResourceT $ send s3.env request
    delete s3 src
    Cache.delete (createCacheKey @"dir" @[File] targetId "")


delete
  :: ( IOE   :> es
     , Log   :> es
     , Cache :> es)
  => Backend S3 -> FilePath -> Eff es ()
delete s3@S3Backend { targetId } filePath = do
  let bucket = Amazonka.BucketName s3.bucket
  let key    = Amazonka.ObjectKey (Text.pack filePath)
  void . runResourceT $ send s3.env (Amazonka.newDeleteObject bucket key)
  Cache.delete (createCacheKey @"file" @File targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"dir" @[File] targetId "")


ls
  :: forall es cacheType cacheName
  . ( Cache :> es
    , Log   :> es
    , IOE   :> es
    , cacheType ~ [File]
    , cacheName ~ "dir")
  => Backend S3 -> FilePath -> Eff es [File]
ls s3@S3Backend { targetId } _ = do
    mCached <- Cache.lookup @cacheType cacheKey
    case mCached of
      Just cached -> do
        pure cached
      Nothing -> do
        let bucket  = Amazonka.BucketName s3.bucket
        let request = Amazonka.newListObjectsV2 bucket
                    & Amazonka.listObjectsV2_prefix ?~ Text.pack "" -- root
        resp <- runResourceT $ send s3.env request
        let files = maybe [] (fmap toFile) $ resp ^. Amazonka.listObjectsV2Response_contents
        let dirs  = maybe [] (fmap toDir)  $ resp ^. Amazonka.listObjectsV2Response_commonPrefixes
        let result = files <> dirs
        Cache.insert
          cacheKey
          (fmap (\r -> SomeCacheKey (createCacheKey @"file" @File targetId (Builder.string8 r.path))) result)
          cacheTTL
          result
        pure result
  where
    cacheKey = createCacheKey @cacheName @cacheType targetId ""
    cacheTTL = Just (secondsToNominalDiffTime 10)

    toDir (commonPrefix :: CommonPrefix) =
      let dirPath = fromMaybe mempty $ commonPrefix ^. Amazonka.commonPrefix_prefix
       in File
         { path     = Text.unpack dirPath
         , atime    = Nothing
         , mtime    = Nothing
         , size     = Nothing
         , mimetype = "" -- content type can be unreliable because it's derived from the extension.
         , content  = Dir
         }

    toFile (object :: Object) =
      let filePath = Amazonka.toText $ object ^. Amazonka.object_key
       in File
         { path     = Text.unpack filePath
         , atime    = Nothing
         , mtime    = Just (object ^. Amazonka.object_lastModified)
         , size     = Just (object ^. Amazonka.object_size)
         , mimetype = defaultMimeLookup filePath -- content type can be unreliable because it's derived from the extension.
         , content  = Content
         }


lsCwd
  :: ( Cache :> es
     , Log   :> es
     , IOE   :> es)
  => Backend S3 -> Eff es [File]
lsCwd s3 = ls s3 ""


upload
  :: ( Cache :> es
     , Log   :> es
     , IOE   :> es)
  => Backend S3 -> MultipartData Mem -> Eff es ()
upload s3 multipart = do
  forM_ multipart.files \file -> do
    let name    = Text.unpack file.fdFileName
    let content = LBS.toStrict (file.fdPayload)
    write s3 name content


download
  :: ( IOE   :> es
     , Log   :> es
     , Cache :> es)
  => Backend S3 -> FilePath -> Eff es (ConduitT () ByteString (ResourceT IO) ())
download s3 path = do
  file     <- get s3 path
  case file.content of
    Content -> readStream s3 file
    Dir -> do
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


-- | Convert a file path to a dir path that ends with /
normalizeDirPath :: FilePath -> FilePath
normalizeDirPath path =
  case uncons (reverse path) of
    Just (l, _) | l /= '/' -> path ++ "/"
    _ -> path
