{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage.S3 (runStorageS3) where

import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Dynamic (throwError)
import Effectful (Eff, Eff, MonadIO (..))
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import Prelude hiding (readFile, writeFile)
import Control.Monad (void)
import Data.List (uncons)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Generics.Labels ()
import Data.Foldable (forM_)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Codec.Archive.Zip qualified as Zip
import Codec.Archive.Zip (ZipOption(..))
import Filehub.Domain (fromClientPath)
import Filehub.Storage.Effect (Storage (..))
import Filehub.Storage.Context qualified as Storage
import Filehub.Domain.Types (File(..), FileContent (..), ClientPath)
import Filehub.Error (FilehubError (..))
import Filehub.Env qualified as Env
import Filehub.Types (SessionId, S3Target(..))
import Filehub.Env.Target (TargetView(..))
import Lens.Micro
import Amazonka (send, runResourceT, toBody)
import Amazonka.S3 (Object(..), CommonPrefix)
import Amazonka.S3 qualified as Amazonka
import Amazonka.S3.Lens qualified as Amazonka
import Amazonka.Data qualified as Amazonka
import Amazonka.Data (sinkBody)
import Network.Mime (defaultMimeLookup)
import Conduit qualified


getFile :: Storage.Context es => SessionId -> FilePath -> Eff es File
getFile sessionId path = do
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


readFileContent :: Storage.Context es => SessionId -> File -> Eff es LBS.ByteString
readFileContent sessionId file = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let key = Amazonka.ObjectKey $ Text.pack file.path
  let request = Amazonka.newGetObject bucket key
  content <- runResourceT do
    resp <- send s3.env request
    content <- liftIO $ sinkBody (resp ^. Amazonka.getObjectResponse_body) Conduit.sinkLazy
    pure content
  pure content


newFolder :: Storage.Context es => SessionId -> FilePath -> Eff es ()
newFolder sessionId filePath = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let key = Amazonka.ObjectKey $ Text.pack $ normalizeDirPath filePath
  let request = Amazonka.newPutObject bucket key (toBody LBS.empty)
  void $ runResourceT $ send s3.env request


newFile :: Storage.Context es => SessionId -> FilePath -> Eff es ()
newFile sessionId filePath = writeFile sessionId filePath mempty


writeFile :: Storage.Context es => SessionId -> FilePath -> LBS.ByteString -> Eff es ()
writeFile sessionId filePath bytes = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let key = Amazonka.ObjectKey $ Text.pack filePath
  let request = Amazonka.newPutObject bucket key (toBody bytes)
  void $ runResourceT $ send s3.env request


deleteFile :: Storage.Context es => SessionId -> FilePath -> Eff es ()
deleteFile sessionId filePath = do
  s3 <- getS3 sessionId
  let bucket = Amazonka.BucketName s3.bucket
  let key = Amazonka.ObjectKey $ Text.pack filePath
  void $ runResourceT $ send s3.env (Amazonka.newDeleteObject bucket key)


lsDir :: Storage.Context es => SessionId -> FilePath -> Eff es [File]
lsDir sessionId path = do
  let normalizedPath = normalizeDirPath path
  isDir <- isDirectory sessionId normalizedPath
  if isDir
     then do
       s3 <- getS3 sessionId
       let bucket = Amazonka.BucketName s3.bucket
       let request = Amazonka.newListObjectsV2 bucket
                   & Amazonka.listObjectsV2_prefix ?~ Text.pack "" -- root
       resp <- runResourceT $ send s3.env request
       let files = maybe [] (fmap toFile) $ resp ^. Amazonka.listObjectsV2Response_contents
       let dirs = maybe [] (fmap toDir) $ resp ^. Amazonka.listObjectsV2Response_commonPrefixes
       pure $ files <> dirs
     else throwError InvalidDir
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


changeDir :: Storage.Context es => SessionId -> FilePath -> Eff es ()
changeDir sessionId path = do
  isDir <- isDirectory sessionId normalizedPath
  if isDir
     then Env.setCurrentDir sessionId normalizedPath
     else throwError InvalidDir
  where
    normalizedPath = normalizeDirPath path


lsCurrentDir :: Storage.Context es => SessionId -> Eff es [File]
lsCurrentDir sessionId = do
  path <- Env.getCurrentDir sessionId
  lsDir sessionId path


upload :: Storage.Context es => SessionId -> MultipartData Mem -> Eff es ()
upload sessionId multipart = do
  forM_ multipart.files $ \file -> do
    let name = Text.unpack file.fdFileName
    let content = file.fdPayload
    writeFile sessionId name content


download :: Storage.Context es => SessionId -> ClientPath -> Eff es LBS.ByteString
download sessionId clientPath = do
  root <- Env.getRoot sessionId
  let abspath = fromClientPath root clientPath
  file <- getFile sessionId abspath
  case file.content of
    Content -> readFileContent sessionId file
    Dir _ -> do
      archive <- liftIO $ Zip.addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] Zip.emptyArchive [file.path]
      pure $ Zip.fromArchive archive



runStorageS3 :: Storage.Context es => SessionId -> Eff (Storage : es) a -> Eff es a
runStorageS3 sessionId = interpret $ \_ -> \case
  GetFile path -> getFile sessionId path
  IsDirectory path -> isDirectory sessionId path
  ReadFileContent file -> readFileContent sessionId file
  NewFolder path -> newFolder sessionId path
  NewFile path -> newFile sessionId path
  WriteFile path bytes -> writeFile sessionId path bytes
  DeleteFile path -> deleteFile sessionId path
  LsDir path -> lsDir sessionId path
  ChangeDir path -> changeDir sessionId path
  LsCurrentDir -> lsCurrentDir sessionId
  Upload multipart -> upload sessionId multipart
  Download clientPath -> download sessionId clientPath


--
-- | Helpers
--


getS3 :: Storage.Context es => SessionId -> Eff es S3Target
getS3 sessionId = do
  TargetView target _ _ <- Env.currentTarget sessionId
  maybe (throwError TargetError) pure $ target ^? #_S3Target


-- | Convert a file path to a dir path that ends with /
normalizeDirPath :: FilePath -> FilePath
normalizeDirPath path =
  case uncons (reverse path) of
    Just (l, _) | l /= '/' -> path ++ "/"
    _ -> path
