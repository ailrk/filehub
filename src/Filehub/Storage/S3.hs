{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
module Filehub.Storage.S3 (storage) where

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
import Data.ClientPath (fromClientPath, ClientPath)
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
import Effectful (Eff, Eff, MonadIO (..), runEff)
import Effectful.Error.Dynamic (throwError)
import Effectful.Extended.Cache qualified as Cache
import Effectful.FileSystem (runFileSystem, removeFile)
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Session qualified as Session
import Filehub.Storage.Context qualified as Storage
import Filehub.Target (TargetView(..), handleTarget, getTargetId)
import Target.S3 (Backend(..), S3)
import Target.Types (targetHandler, TargetId)
import Target.Types.TargetId qualified as TargetId
import Filehub.Types (SessionId)
import GHC.TypeLits (Symbol)
import Lens.Micro
import Lens.Micro.Platform ()
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import System.IO.Temp qualified as Temp
import Filehub.Storage.Types (Storage(..))


class CacheKeyComponent (s :: Symbol) a              where toCacheKeyComponent :: Builder
instance CacheKeyComponent "file"         File       where toCacheKeyComponent = "f:"
instance CacheKeyComponent "dir"          [File]     where toCacheKeyComponent = "d:"
instance CacheKeyComponent "file-content" ByteString where toCacheKeyComponent = "fcc:"
instance CacheKeyComponent "is-directory" Bool       where toCacheKeyComponent = "id:"



cacheKeyPrefix :: Builder
cacheKeyPrefix = "st:s3:"


createCacheKey :: forall (s :: Symbol) (a :: Type) . CacheKeyComponent s a => TargetId -> Builder -> CacheKey
createCacheKey targetId identifier = Cache.mkCacheKey
  [cacheKeyPrefix, TargetId.targetIdBuilder targetId, ":", toCacheKeyComponent @s @a, identifier]


get :: forall es cacheType cacheName
    . (Storage.Context es, cacheType ~ File, cacheName ~ "file")
    => SessionId -> FilePath -> Eff es File
get sessionId path = do
  (targetId, s3) <- getS3 sessionId
  let cacheKey   =  createCacheKey @cacheName @cacheType targetId (Builder.string8 path)
  mCached        <- Cache.lookup @File cacheKey
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
    cacheTTL = Just (secondsToNominalDiffTime 10)


-- | Because S3 doesn't have real directory, we need to list all keys in the
-- bucket and check if the file path is prefix of any key.
isDirectory :: forall es cacheType cacheName
            . (Storage.Context es, cacheType ~ Bool, cacheName ~ "is-directory")
            => SessionId -> FilePath -> Eff es Bool
isDirectory sessionId filePath = do
  (targetId, s3) <- getS3 sessionId
  let cacheKey = createCacheKey @cacheName @cacheType targetId (Builder.string8 filePath)
  mCached <- Cache.lookup @cacheType cacheKey
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
    cacheTTL = Just (secondsToNominalDiffTime 10)


read :: forall es cacheType cacheName
     . (Storage.Context es, cacheType ~ ByteString, cacheName ~ "file-content")
     => SessionId -> File -> Eff es ByteString
read sessionId file = do
  (targetId, _) <- getS3 sessionId
  let cacheKey = createCacheKey @cacheName @cacheType targetId (Builder.string8 file.path)
  mCached <- Cache.lookup @cacheType cacheKey
  case mCached of
    Just cached -> pure cached
    Nothing -> do
      stream <- readStream sessionId file
      chunks <- liftIO $ runResourceT . Conduit.runConduit $ stream Conduit..| Conduit.sinkList
      let result = LBS.toStrict (LBS.fromChunks chunks)
      Cache.insert cacheKey cacheTTL result
      pure result
  where
    cacheTTL = Just (secondsToNominalDiffTime 10)


readStream :: Storage.Context es => SessionId -> File -> Eff es (ConduitT () ByteString (ResourceT IO) ())
readStream sessionId file = do
  (_, s3) <- getS3 sessionId
  let bucket  = Amazonka.BucketName s3.bucket
      key     = Amazonka.ObjectKey (Text.pack file.path)
      request = Amazonka.newGetObject bucket key
  pure $ do
    resp <- lift $ send s3.env request
    let (ResponseBody conduit) = resp ^. Amazonka.getObjectResponse_body
    conduit


newFolder :: Storage.Context es => SessionId -> FilePath -> Eff es ()
newFolder sessionId filePath = do
  (targetId, s3) <- getS3 sessionId
  let bucket  = Amazonka.BucketName s3.bucket
  let key     = Amazonka.ObjectKey (Text.pack (normalizeDirPath filePath))
  let request = Amazonka.newPutObject bucket key (toBody LBS.empty)
  void . runResourceT $ send s3.env request
  Cache.delete (createCacheKey @"file"         @File       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"file-content" @ByteString targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"is-directory" @Bool       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"dir"          @[File]     targetId (Builder.string8 filePath))


new :: Storage.Context es => SessionId -> FilePath -> Eff es ()
new sessionId filePath = write sessionId filePath mempty


write :: Storage.Context es => SessionId -> FilePath -> ByteString -> Eff es ()
write sessionId filePath bytes = do
  (targetId, s3) <- getS3 sessionId
  let bucket  = Amazonka.BucketName s3.bucket
  let key     = Amazonka.ObjectKey (Text.pack filePath)
  let request = Amazonka.newPutObject bucket key (toBody bytes)
  void . runResourceT $ send s3.env request
  Cache.delete (createCacheKey @"file"         @File       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"file-content" @ByteString targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"is-directory" @Bool       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"dir"          @[File]     targetId (Builder.string8 filePath))


cp :: Storage.Context es => SessionId -> FilePath -> FilePath -> Eff es ()
cp sessionId src dst = do
  (targetId, s3) <- getS3 sessionId
  let bucket  = Amazonka.BucketName s3.bucket
  let destKey = Amazonka.ObjectKey (Text.pack dst)
  let request = Amazonka.newCopyObject bucket (Text.pack src) destKey
  void . runResourceT $ send s3.env request
  Cache.delete (createCacheKey @"dir" @[File] targetId (Builder.string8 dst))


delete :: Storage.Context es => SessionId -> FilePath -> Eff es ()
delete sessionId filePath = do
  (targetId, s3) <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let key    = Amazonka.ObjectKey (Text.pack filePath)
  void . runResourceT $ send s3.env (Amazonka.newDeleteObject bucket key)
  Cache.delete (createCacheKey @"file"         @File       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"file-content" @ByteString targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"is-directory" @Bool       targetId (Builder.string8 filePath))
  Cache.delete (createCacheKey @"dir"          @[File]     targetId (Builder.string8 filePath))


ls :: forall es cacheType cacheName
    . (Storage.Context es, cacheType ~ [File], cacheName ~ "dir")
    => SessionId -> FilePath -> Eff es [File]
ls sessionId _ = do
    (targetId, s3) <- getS3 sessionId
    let cacheKey = createCacheKey @cacheName @cacheType targetId ""
    mCached <- Cache.lookup @[File] cacheKey
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
        Cache.insert cacheKey Nothing result
        pure result
  where
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


cd :: SessionId -> FilePath -> Eff es ()
cd _ _ = pure ()


lsCwd :: Storage.Context es => SessionId -> Eff es [File]
lsCwd sessionId = ls sessionId ""


upload :: Storage.Context es => SessionId -> MultipartData Mem -> Eff es ()
upload sessionId multipart = do
  forM_ multipart.files \file -> do
    let name    = Text.unpack file.fdFileName
    let content = LBS.toStrict (file.fdPayload)
    write sessionId name content


download :: Storage.Context es => SessionId -> ClientPath -> Eff es (ConduitT () ByteString (ResourceT IO) ())
download sessionId clientPath = do
  root <- Session.getRoot sessionId
  let path = fromClientPath root clientPath
  file <- get sessionId path
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


getS3 :: Storage.Context es => SessionId -> Eff es (TargetId, Backend S3)
getS3 sessionId = do
  TargetView target _ _ <- Session.currentTarget sessionId
  maybe (throwError (FilehubError TargetError "Target is not valid S3 bucket")) pure $ handleTarget target
    [ targetHandler @S3 \x -> (getTargetId target, x)
    ]


-- | Convert a file path to a dir path that ends with /
normalizeDirPath :: FilePath -> FilePath
normalizeDirPath path =
  case uncons (reverse path) of
    Just (l, _) | l /= '/' -> path ++ "/"
    _ -> path
