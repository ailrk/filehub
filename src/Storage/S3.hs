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
module Storage.S3 where

import Amazonka (send, runResourceT, toBody, ResponseBody (..))
import Amazonka.Data qualified as Amazonka
import Amazonka.S3 (Object(..), CommonPrefix)
import Amazonka.S3 qualified as Amazonka
import Amazonka.S3.Lens qualified as Amazonka
import Cache.Key (CacheKey)
import Codec.Archive.Zip qualified as Zip
import Conduit (ConduitT, ResourceT, MonadTrans (..))
import Conduit qualified
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.File (File (..), FileContent (..))
import Data.Foldable (forM_)
import Data.Generics.Labels ()
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (secondsToNominalDiffTime)
import Effectful (Eff, Eff, MonadIO (..), runEff, (:>), IOE)
import Effectful.Extended.Cache qualified as Cache
import Effectful.FileSystem (runFileSystem, removeFile)
import Target.S3 (Backend(..), S3)
import Target.Types (TargetId)
import Target.Types.TargetId qualified as TargetId
import GHC.TypeLits (Symbol)
import Lens.Micro
import Lens.Micro.Platform ()
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import System.IO.Temp qualified as Temp
import Effectful.Extended.Cache (Cache)
import Effectful.Log (Log)


class CacheKeyComponent (s :: Symbol) a              where toCacheKeyComponent :: Builder
instance CacheKeyComponent "file"         File       where toCacheKeyComponent = "f"
instance CacheKeyComponent "dir"          [File]     where toCacheKeyComponent = "d"
instance CacheKeyComponent "file-content" ByteString where toCacheKeyComponent = "fc"
instance CacheKeyComponent "is-directory" Bool       where toCacheKeyComponent = "id"


cacheKeyPrefix :: Builder
cacheKeyPrefix = "st:s3:"


createCacheKey :: forall (s :: Symbol) (a :: Type) . CacheKeyComponent s a => TargetId -> Builder -> CacheKey
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
      Cache.insert cacheKey cacheTTL file
      pure file
  where
    cacheKey =  createCacheKey @cacheName @cacheType targetId (Builder.string8 path)
    cacheTTL = Just (secondsToNominalDiffTime 10)


-- | Because S3 doesn't have real directory, we need to list all keys in the
-- bucket and check if the file path is prefix of any key.
isDirectory
  :: forall es cacheType cacheName
  . ( Cache :> es
    , Log   :> es
    , IOE   :> es
    , cacheType ~ Bool
    , cacheName ~ "is-directory")
  => Backend S3 -> FilePath -> Eff es Bool
isDirectory s3@S3Backend { targetId } filePath = do
  mCached      <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      let bucket = Amazonka.BucketName s3.bucket
      let request = Amazonka.newListObjectsV2 bucket
                  & Amazonka.listObjectsV2_prefix ?~ Text.pack (normalizeDirPath filePath)
                  & Amazonka.listObjectsV2_maxKeys ?~ 1
      resp <- runResourceT $ send s3.env request
      let result = maybe False (> 0) (resp ^. Amazonka.listObjectsV2Response_keyCount)
      Cache.insert cacheKey cacheTTL result
      pure result
  where
    cacheKey =  createCacheKey @cacheName @cacheType targetId (Builder.string8 filePath)
    cacheTTL = Just (secondsToNominalDiffTime 10)


read
  :: forall es cacheType cacheName
  . ( IOE   :> es
    , Log   :> es
    , Cache :> es
    , cacheType ~ ByteString
    , cacheName ~ "file-content")
  => Backend S3 -> File -> Eff es ByteString
read s3@S3Backend { targetId }  file = do
  mCached      <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      stream <- readStream s3 file
      chunks <- liftIO $ runResourceT . Conduit.runConduit $ stream Conduit..| Conduit.sinkList
      let result = LBS.toStrict (LBS.fromChunks chunks)
      Cache.insert cacheKey cacheTTL result
      pure result
  where
    cacheKey = createCacheKey @cacheName @cacheType targetId (Builder.string8 file.path)
    cacheTTL = Just (secondsToNominalDiffTime 10)


readStream :: Backend S3 -> File -> Eff es (ConduitT () ByteString (ResourceT IO) ())
readStream s3 file = do
  let bucket  = Amazonka.BucketName s3.bucket
      key     = Amazonka.ObjectKey (Text.pack file.path)
      request = Amazonka.newGetObject bucket key
  pure $ do
    resp <- lift $ send s3.env request
    let (ResponseBody conduit) = resp ^. Amazonka.getObjectResponse_body
    conduit


newFolder
  :: ( Cache :> es
     , Log   :> es
     , IOE   :> es)
  => Backend S3 -> FilePath -> Eff es ()
newFolder s3@S3Backend { targetId } filePath = do
  let bucket  = Amazonka.BucketName s3.bucket
  let key     = Amazonka.ObjectKey (Text.pack (normalizeDirPath filePath))
  let request = Amazonka.newPutObject bucket key (toBody LBS.empty)
  void . runResourceT $ send s3.env request
  Cache.delete (createCacheKey @"file"         @File       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"file-content" @ByteString targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"is-directory" @Bool       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"dir"          @[File]     targetId (Builder.string8 ""))


new
  :: ( Cache :> es
     , Log   :> es
     , IOE   :> es)
  => Backend S3 -> FilePath -> Eff es ()
new s3 filePath = write s3 filePath mempty


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
  Cache.delete (createCacheKey @"file"         @File       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"file-content" @ByteString targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"is-directory" @Bool       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"dir"          @[File]     targetId (Builder.string8 ""))


cp
  :: ( IOE   :> es
     , Log   :> es
     , Cache :> es)
   => Backend S3 -> FilePath -> FilePath -> Eff es ()
cp s3@S3Backend { targetId } src dst = do
  let bucket  = Amazonka.BucketName s3.bucket
  let destKey = Amazonka.ObjectKey (Text.pack dst)
  let request = Amazonka.newCopyObject bucket (Text.pack src) destKey
  void . runResourceT $ send s3.env request
  Cache.delete (createCacheKey @"dir" @[File] targetId (Builder.string8 dst))


delete
  :: ( IOE   :> es
     , Log   :> es
     , Cache :> es)
  => Backend S3 -> FilePath -> Eff es ()
delete s3@S3Backend { targetId } filePath = do
  let bucket = Amazonka.BucketName s3.bucket
  let key    = Amazonka.ObjectKey (Text.pack filePath)
  void . runResourceT $ send s3.env (Amazonka.newDeleteObject bucket key)
  Cache.delete (createCacheKey @"file"         @File       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"file-content" @ByteString targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"is-directory" @Bool       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"dir"          @[File]     targetId (Builder.string8 ""))


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
        Cache.insert cacheKey cacheTTL result
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
         , content  = Dir Nothing
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
  :: ( IOE        :> es
     , Log        :> es
     , Cache      :> es)
  => Backend S3 -> FilePath -> Eff es (ConduitT () ByteString (ResourceT IO) ())
download s3 path = do
  file     <- get s3 path
  case file.content of
    Content -> readStream s3 file
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


--
-- | Helpers
--


-- | Convert a file path to a dir path that ends with /
normalizeDirPath :: FilePath -> FilePath
normalizeDirPath path =
  case uncons (reverse path) of
    Just (l, _) | l /= '/' -> path ++ "/"
    _ -> path
