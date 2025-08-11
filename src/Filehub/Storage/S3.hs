{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage.S3 (storage) where

import Codec.Archive.Zip (ZipOption(..))
import Codec.Archive.Zip qualified as Zip
import Amazonka (send, runResourceT, toBody, ResponseBody (..))
import Amazonka.Data qualified as Amazonka
import Amazonka.S3 (Object(..), CommonPrefix)
import Amazonka.S3 qualified as Amazonka
import Amazonka.S3.Lens qualified as Amazonka
import Conduit qualified
import Control.Monad (void)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (forM_)
import Data.Generics.Labels ()
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful (Eff, Eff, MonadIO (..))
import Effectful.Error.Dynamic (throwError)
import Filehub.ClientPath (fromClientPath)
import Filehub.Env qualified as Env
import Filehub.Target (TargetView(..), handleTarget)
import Filehub.Error (FilehubError (..))
import Filehub.Target.Types (Storage(..))
import Filehub.Types (File(..), FileContent(..), ClientPath, SessionId)
import Lens.Micro
import Network.Mime (defaultMimeLookup)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import Data.Generics.Labels ()
import Lens.Micro.Platform ()
import Conduit (ConduitT, ResourceT, MonadTrans (..), sourceLazy)
import Data.ByteString (ByteString)
import Filehub.Target.S3 (Backend(..), S3)
import Filehub.Target.Types (targetHandler)
import Filehub.Storage.Context qualified as Storage


get :: Storage.Context es => SessionId -> FilePath -> Eff es File
get sessionId path = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let key = Amazonka.ObjectKey $ Text.pack path
  let request = Amazonka.newHeadObject bucket key
  resp <- runResourceT $ send s3.env request
  let mtime = resp ^. Amazonka.headObjectResponse_lastModified
  let size = resp ^. Amazonka.headObjectResponse_contentLength
  let contentType = resp ^. Amazonka.headObjectResponse_contentType
  pure File
    { path = path
    , atime = Nothing
    , mtime = mtime
    , size = size
    , mimetype = maybe "application/octet-stream" Text.encodeUtf8 contentType
    , content = Content
    }


-- | Because S3 doesn't have real directory, we need to list all keys in the
-- bucket and check if the file path is prefix of any key.
isDirectory :: Storage.Context es => SessionId -> FilePath -> Eff es Bool
isDirectory sessionId filePath = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let request = Amazonka.newListObjectsV2 bucket
              & Amazonka.listObjectsV2_prefix ?~ Text.pack (normalizeDirPath filePath)
              & Amazonka.listObjectsV2_maxKeys ?~ 1
  resp <- runResourceT $ send s3.env request
  pure $ maybe False (> 0) (resp ^. Amazonka.listObjectsV2Response_keyCount)


read :: Storage.Context es => SessionId -> File -> Eff es ByteString
read sessionId file = do
  stream <- readStream sessionId file
  chunks <- liftIO $ runResourceT . Conduit.runConduit $ stream Conduit..| Conduit.sinkList
  pure $ LBS.toStrict $ LBS.fromChunks chunks


readStream :: Storage.Context es => SessionId -> File -> Eff es (ConduitT () ByteString (ResourceT IO) ())
readStream sessionId file = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
      key = Amazonka.ObjectKey $ Text.pack file.path
      request = Amazonka.newGetObject bucket key
  pure $ do
    resp <- lift $ send s3.env request
    let (ResponseBody conduit) = resp ^. Amazonka.getObjectResponse_body
    conduit


newFolder :: Storage.Context es => SessionId -> FilePath -> Eff es ()
newFolder sessionId filePath = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let key = Amazonka.ObjectKey $ Text.pack $ normalizeDirPath filePath
  let request = Amazonka.newPutObject bucket key (toBody LBS.empty)
  void $ runResourceT $ send s3.env request


new :: Storage.Context es => SessionId -> FilePath -> Eff es ()
new sessionId filePath = write sessionId filePath mempty


write :: Storage.Context es => SessionId -> FilePath -> ByteString -> Eff es ()
write sessionId filePath bytes = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let key = Amazonka.ObjectKey $ Text.pack filePath
  let request = Amazonka.newPutObject bucket key (toBody bytes)
  void $ runResourceT $ send s3.env request


delete :: Storage.Context es => SessionId -> FilePath -> Eff es ()
delete sessionId filePath = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let key = Amazonka.ObjectKey $ Text.pack filePath
  void $ runResourceT $ send s3.env (Amazonka.newDeleteObject bucket key)


ls :: Storage.Context es => SessionId -> FilePath -> Eff es [File]
ls sessionId _ = do
   s3 <- getS3 sessionId
   let bucket = Amazonka.BucketName s3.bucket
   let request = Amazonka.newListObjectsV2 bucket
               & Amazonka.listObjectsV2_prefix ?~ Text.pack "" -- root
   resp <- runResourceT $ send s3.env request
   let files = maybe [] (fmap toFile) $ resp ^. Amazonka.listObjectsV2Response_contents
   let dirs = maybe [] (fmap toDir) $ resp ^. Amazonka.listObjectsV2Response_commonPrefixes
   pure $ files <> dirs
  where
    toDir (commonPrefix :: CommonPrefix) =
      let dirPath = fromMaybe mempty $ commonPrefix ^. Amazonka.commonPrefix_prefix
       in File
         { path = Text.unpack dirPath
         , atime = Nothing
         , mtime = Nothing
         , size = Nothing
         , mimetype = "" -- content type can be unreliable because it's derived from the extension.
         , content = Dir Nothing
         }

    toFile (object :: Object) =
      let filePath = Amazonka.toText $ object ^. Amazonka.object_key
       in File
         { path = Text.unpack filePath
         , atime = Nothing
         , mtime = Just $ object ^. Amazonka.object_lastModified
         , size = Just $ object ^. Amazonka.object_size
         , mimetype = defaultMimeLookup filePath -- content type can be unreliable because it's derived from the extension.
         , content = Content
         }


cd :: SessionId -> FilePath -> Eff es ()
cd _ _ = pure ()


lsCwd :: Storage.Context es => SessionId -> Eff es [File]
lsCwd sessionId = ls sessionId ""


upload :: Storage.Context es => SessionId -> MultipartData Mem -> Eff es ()
upload sessionId multipart = do
  forM_ multipart.files $ \file -> do
    let name = Text.unpack file.fdFileName
    let content = LBS.toStrict $ file.fdPayload
    write sessionId name content


download :: Storage.Context es => SessionId -> ClientPath -> Eff es (ConduitT () ByteString (ResourceT IO) ())
download sessionId clientPath = do
  root <- Env.getRoot sessionId
  let abspath = fromClientPath root clientPath
  file <- get sessionId abspath
  case file.content of
    Content -> readStream sessionId file
    Dir _ -> do
      archive <- liftIO $ Zip.addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] Zip.emptyArchive [file.path]
      pure . sourceLazy $ Zip.fromArchive archive


storage :: Storage.Context es => SessionId -> (Storage (Eff es))
storage sessionId =
  Storage
    { get = get sessionId
    , read = read sessionId
    , readStream = readStream sessionId
    , write = write sessionId
    , delete = delete sessionId
    , new = new sessionId
    , newFolder = newFolder sessionId
    , ls = ls sessionId
    , cd = cd sessionId
    , lsCwd = lsCwd sessionId
    , upload = upload sessionId
    , download = download sessionId
    , isDirectory = isDirectory sessionId
    }


--
-- | Helpers
--


getS3 :: Storage.Context es => SessionId -> Eff es (Backend S3)
getS3 sessionId = do
  TargetView target _ _ <- Env.currentTarget sessionId
  maybe (throwError TargetError) pure $ handleTarget target
    [ targetHandler @S3 $ \x -> x
    ]


-- | Convert a file path to a dir path that ends with /
normalizeDirPath :: FilePath -> FilePath
normalizeDirPath path =
  case uncons (reverse path) of
    Just (l, _) | l /= '/' -> path ++ "/"
    _ -> path
