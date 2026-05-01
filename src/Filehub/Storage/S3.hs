{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
module Filehub.Storage.S3 where

import Amazonka (send, runResourceT, toBody, ResponseBody (..), RequestBody)
import Amazonka.Data qualified as Amazonka
import Amazonka.S3 (Object(..), CommonPrefix, CompletedMultipartUpload (CompletedMultipartUpload'), BucketName (..), ObjectKey (..))
import Amazonka.S3 qualified as Amazonka
import Amazonka.S3.CompleteMultipartUpload (CompleteMultipartUpload(..))
import Amazonka.S3.CopyObject (CopyObjectResponse(..))
import Amazonka.S3.CreateMultipartUpload (CreateMultipartUpload(..), CreateMultipartUploadResponse(..))
import Amazonka.S3.Lens qualified as Amazonka
import Amazonka.S3.PutObject (PutObject(..))
import Amazonka.S3.UploadPart (UploadPartResponse(..))
import Cache.Key (CacheKey, SomeCacheKey (..))
import Codec.Archive.Zip qualified as Zip
import Conduit (ResourceT, MonadTrans (..), sinkLazy)
import Conduit qualified
import Control.Monad (void)
import Control.Service.Cache (MonadCache(..))
import Control.Service.Cache qualified as Cache
import Control.Service.LockManager qualified as LockManager
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.ClientPath (AbsPath (..))
import Data.Coerce (coerce)
import Data.Conduit
import Data.File (File (..), FileType (..), FileInfo, FileWithContent, FileContent (..), defaultFileWithContent, IsLink (..))
import Data.Function (fix)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List (sort)
import Data.List (uncons)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (secondsToNominalDiffTime)
import Filehub.Error
import Filehub.Monad (Filehub)
import GHC.TypeLits (Symbol)
import Lens.Micro
import Lens.Micro.Platform ()
import Lens.Micro.Platform ()
import Log (logAttention_)
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (Mem, FileData (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp qualified as Temp
import Target.S3 (Target(..), S3)
import Target.Types (TargetId)
import Target.Types qualified as Target
import UnliftIO (MonadIO (..), throwIO)
import UnliftIO.Async (forConcurrently_)
import UnliftIO.Directory (removeFile)
import Amazonka.S3.GetObject (GetObject(..))
import Data.ByteString.Char8 qualified as Char8


class CacheKeyComponent (s :: Symbol) a              where toCacheKeyComponent :: Builder
instance CacheKeyComponent "file"         FileInfo   where toCacheKeyComponent = "f"
instance CacheKeyComponent "dir"          [FileInfo] where toCacheKeyComponent = "d"
instance CacheKeyComponent "file-content" ByteString where toCacheKeyComponent = "fc"
instance CacheKeyComponent "is-directory" Bool       where toCacheKeyComponent = "id"


cacheKeyPrefix :: Builder
cacheKeyPrefix = "st:s3:"


createCacheKey :: forall (s :: Symbol) (a :: Type) . CacheKeyComponent s a => TargetId -> Builder -> CacheKey a
createCacheKey targetId identifier = Cache.mkCacheKey
  [cacheKeyPrefix, Target.targetIdBuilder targetId, toCacheKeyComponent @s @a, identifier]


get :: Target S3 -> AbsPath -> Filehub FileInfo
get (s3@S3Backend { targetId }) path = do
  mCached <- cacheLookup cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      let bucket  = BucketName s3.bucket
          key     = ObjectKey (coerce Text.pack path)
          request = Amazonka.newHeadObject bucket key
      resp <- send s3.env request
      if resp ^. Amazonka.headObjectResponse_httpStatus == 200
         then do
          let mtime       = resp ^. Amazonka.headObjectResponse_lastModified
              size        = resp ^. Amazonka.headObjectResponse_contentLength
              contentType = resp ^. Amazonka.headObjectResponse_contentType
              file = File
                { path     = path
                , atime    = Nothing
                , mtime    = mtime
                , size     = size
                , mimetype = maybe "application/octet-stream" Text.encodeUtf8 contentType
                , isLink   = NotLink
                , content  = Regular
                }
          cacheInsert cacheKey cacheDeps cacheTTL file
          pure file
        else do
          throwIO (FilehubError InvalidPath "invalid path")
  where
    cacheKey  =  createCacheKey @"file" @FileInfo targetId (coerce Builder.string8 path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"file" @FileInfo targetId "") ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


-- | Because S3 doesn't have real directory, we need to list all keys in the
-- bucket and check if the file path is prefix of any key.
isDirectory :: Target S3 -> AbsPath -> Filehub Bool
isDirectory s3@S3Backend { targetId } filePath = do
  mCached <- cacheLookup cacheKey
  case mCached of
    Just (File { content = Dir }) -> pure True
    Just _                        -> pure False
    Nothing -> do
      let bucket  = BucketName s3.bucket
          request = Amazonka.newListObjectsV2 bucket
                  & Amazonka.listObjectsV2_prefix ?~ Text.pack (coerce normalizeDirPath filePath)
                  & Amazonka.listObjectsV2_maxKeys ?~ 1
      resp <- send s3.env request
      let result = maybe False (> 0) (resp ^. Amazonka.listObjectsV2Response_keyCount)
      pure result
  where
    cacheKey = createCacheKey @"file" @FileInfo targetId (coerce Builder.string8 filePath)


read :: Target S3 -> FileInfo -> Filehub ByteString
read s3@S3Backend { targetId }  file = do
  mCached <- cacheLookup cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      stream <- readStream s3 file Nothing Nothing
      chunks <- liftIO $ runResourceT . Conduit.runConduit $ stream Conduit..| Conduit.sinkList
      let result = LBS.toStrict (LBS.fromChunks chunks)
      cacheInsert cacheKey cacheDeps cacheTTL result
      pure result
  where
    cacheKey  = createCacheKey @"file-content" @ByteString targetId (coerce Builder.string8 file.path)
    cacheDeps = [ SomeCacheKey (createCacheKey @"file" @FileInfo targetId (coerce Builder.string8 file.path)) ]
    cacheTTL  = Just (secondsToNominalDiffTime 10)


readStream :: Target S3
           -> FileInfo
           -> Maybe Integer -- ^ Offest
           -> Maybe Integer -- ^ Max Bytes
           -> Filehub (ConduitT () ByteString (ResourceT IO) ())
readStream s3 file mOff mMax = do
  let bucket  = BucketName s3.bucket
      key     = ObjectKey (coerce Text.pack file.path)
      range   = case (mOff, mMax) of
                  (Just off, Just len) -> Just $ mconcat [ "bytes="
                                                         , Char8.pack (show off)
                                                         , "-"
                                                         , Char8.pack (show (off + len - 1))
                                                         ]
                  (Just off, Nothing)  -> Just $ mconcat [ "bytes="
                                                         , Char8.pack (show off)
                                                         , "-"
                                                         ]
                  (Nothing, Just len)  -> Just $ mconcat [ "bytes=0-"
                                                         , Char8.pack (show (len - 1)) -- be careful with off by 1
                                                         ]
                  (Nothing, Nothing)   -> Nothing
      request = (Amazonka.newGetObject bucket key) { range = Text.decodeUtf8 <$> range }
  pure $ do
    resp <- lift $ send s3.env request
    let (ResponseBody conduit) = resp ^. Amazonka.getObjectResponse_body
    conduit


new :: Target S3 -> AbsPath -> Filehub FileInfo
new s3@S3Backend { targetId } path = do
  write s3 $ defaultFileWithContent
    { path     = path
    , mimetype = "text/plain"
    , content  = FileContentRaw ""
    }

  cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] targetId ""))

  file <- get s3 path
  pure file


write :: Target S3 -> FileWithContent -> Filehub ()
write s3@S3Backend { targetId } File { content, mimetype, size = mSize, path } =
  LockManager.withLock (LockManager.mkLockKey path) do
    case content of
      FileContentRaw bytes -> do
        writePutObject path (toBody bytes)
        cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo targetId (coerce Builder.string8 path)))
        cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] targetId ""))
      FileContentConduit conduit -> do
        case mSize of
          Nothing -> writeMultipart conduit
          Just size
            | size < threshold  ->  do
                lazyBytes <- liftIO . runResourceT . runConduit $ conduit .| sinkLazy
                writePutObject path (toBody lazyBytes)
            | otherwise -> writeMultipart conduit
        cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo targetId (coerce Builder.string8 path)))
        cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] targetId ""))
        where
          threshold = 5 * 1024 * 1024 -- use putObject if it's smaller than single part.
      FileContentDir _ -> pure ()
      FileContentNull -> pure ()
  where
    writePutObject :: AbsPath -> RequestBody -> Filehub ()
    writePutObject filePath body =
      let bucket  = BucketName s3.bucket
          key     = ObjectKey (coerce Text.pack filePath)
          request = (Amazonka.newPutObject bucket key body) { contentType = Just (Text.decodeUtf8 mimetype) } :: PutObject
       in void $ send s3.env request

    writeMultipart :: ConduitT () ByteString (ResourceT IO) () -> Filehub ()
    writeMultipart conduit = do
      let bucket   = BucketName s3.bucket
          key      = ObjectKey (coerce Text.pack path)
          partSize = 5 * 1024 * 1024
          request  = (Amazonka.newCreateMultipartUpload bucket key) { contentType = Just (Text.decodeUtf8 mimetype) } :: CreateMultipartUpload
      createMultipartUploadResp <- send s3.env request
      let uploadId = createMultipartUploadResp.uploadId
      let mkCompletedPart = \loop -> do
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
             Nothing -> pure ()
      completedParts <- liftIO . runResourceT . runConduit
        $ conduit
        .| chunking partSize
        .| fix mkCompletedPart
        .| Conduit.sinkList
      void $ send s3.env
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
            | size /= 0 -> yield (idx, acc) -- done
            | otherwise       -> pure ()
          ChunkBuilded _ -> pure ()
      Just bytes -> do
        let combined  = acc <> ChunkBuilder (Builder.byteString bytes) (ByteString.length bytes)
        let cbSize    = chunkBuilderSize combined
        if cbSize >= chunkSize
          then do
            let combinedBytes = chunkBuilderToByteString combined
            let (chunk, rest) = ByteString.splitAt chunkSize combinedBytes
            yield (idx, ChunkBuilded chunk)
            rec (idx + 1, ChunkBuilder (Builder.byteString rest) (cbSize - chunkSize))
          else
            rec (idx, combined)


mv :: Target S3 -> [(AbsPath, AbsPath)] -> Filehub ()
mv _ [] = throwIO (FilehubError CopyError "Nothing to copy")
mv s3@S3Backend { targetId } cpPairs = do
  let pairs = sort cpPairs
  forConcurrently_  pairs \(src, dst) -> do
    let locks = if src == dst -- Dedup
                   then [LockManager.mkLockKey src]
                   else (fmap LockManager.mkLockKey [src, dst])
    LockManager.withLocks locks do
      let bucket  = BucketName s3.bucket
          destKey = ObjectKey (coerce Text.pack dst)
          request = Amazonka.newCopyObject bucket (coerce Text.pack src) destKey
      resp <- send s3.env request
      case resp.copyObjectResult of
        Just _ -> do
          cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] targetId (Builder.string8 (coerce takeDirectory src))))
          cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] targetId (Builder.string8 (coerce takeDirectory dst))))
          coerce delete' s3 src
        Nothing ->
          logAttention_ (Text.pack $ "S3 Copy Failed for " <> (coerce src))


rename :: Target S3 -> AbsPath -> String -> Filehub ()
rename s3 oldPath newName =
  let dir     = coerce takeDirectory oldPath
      newPath = AbsPath (dir </> newName) -- @TODO need to avoid this
   in mv s3 [(oldPath, newPath)]


delete :: Target S3 -> AbsPath -> Filehub ()
delete s3 filePath = LockManager.withLock (LockManager.mkLockKey filePath) do
  delete' s3 filePath


delete' :: Target S3 -> AbsPath -> Filehub ()
delete' s3@S3Backend { targetId } filePath = do
  let bucket = BucketName s3.bucket
      key    = ObjectKey (coerce Text.pack filePath)
  void $ send s3.env (Amazonka.newDeleteObject bucket key)
  cacheDelete (SomeCacheKey (createCacheKey @"file" @FileInfo targetId (coerce Builder.string8 filePath)))
  cacheDelete (SomeCacheKey (createCacheKey @"dir" @[FileInfo] targetId ""))


ls :: Target S3 -> AbsPath -> Filehub [FileInfo]
ls s3@S3Backend { targetId } _ = do
    mCached <- cacheLookup cacheKey
    case mCached of
      Just cached -> do
        pure cached
      Nothing -> do
        let bucket  = BucketName s3.bucket
            request = Amazonka.newListObjectsV2 bucket
                    & Amazonka.listObjectsV2_prefix ?~ Text.pack "" -- root
        resp <- send s3.env request
        let files  = maybe [] (fmap toFile) $ resp ^. Amazonka.listObjectsV2Response_contents
            dirs   = maybe [] (fmap toDir)  $ resp ^. Amazonka.listObjectsV2Response_commonPrefixes
            result = files <> dirs
        cacheInsert
          cacheKey
          (fmap (\r -> SomeCacheKey (createCacheKey @"file" @FileInfo targetId (coerce Builder.string8 r.path))) result)
          cacheTTL
          result
        pure result
  where
    cacheKey = createCacheKey @"dir" @[FileInfo] targetId ""
    cacheTTL = Just (secondsToNominalDiffTime 10)

    toDir (commonPrefix :: CommonPrefix) =
      let dirPath = fromMaybe mempty $ commonPrefix ^. Amazonka.commonPrefix_prefix
       in File
         { path     = coerce Text.unpack dirPath
         , atime    = Nothing
         , mtime    = Nothing
         , size     = Nothing
         , mimetype = "" -- content type can be unreliable because it's derived from the extension.
         , isLink   = NotLink
         , content  = Dir
         }

    toFile (object :: Object) =
      let filePath = Amazonka.toText $ object ^. Amazonka.object_key
       in File
         { path     = coerce Text.unpack filePath
         , atime    = Nothing
         , mtime    = Just (object ^. Amazonka.object_lastModified)
         , size     = Just (object ^. Amazonka.object_size)
         , mimetype = defaultMimeLookup filePath -- content type can be unreliable because it's derived from the extension.
         , isLink   = NotLink
         , content  = Regular
         }


lsCwd :: Target S3 -> Filehub [FileInfo]
lsCwd s3 = ls s3 (AbsPath "")


upload :: Target S3 -> FileData Mem -> Filehub ()
upload s3 file = do
  let mimetype = Text.encodeUtf8 file.fdFileCType
      name     = coerce Text.unpack file.fdFileName
      bytes    = LBS.toStrict (file.fdPayload)
  write s3 $ defaultFileWithContent
    { path     = name
    , mimetype = mimetype
    , content  = FileContentRaw bytes
    }


download :: Target S3 -> AbsPath -> Filehub (ConduitT () ByteString (ResourceT IO) ())
download s3 path = do
  file <- get s3 path

  case file.isLink of
    BrokenLink -> throwIO (FilehubError InvalidPath "broken link")
    _ -> pure ()

  case file.content of
    Regular    -> readStream s3 file Nothing Nothing
    Dir        -> do
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


-- | Convert a file path to a dir path that ends with /
normalizeDirPath :: FilePath -> FilePath
normalizeDirPath path =
  case uncons (reverse path) of
    Just (l, _) | l /= '/' -> path ++ "/"
    _ -> path
