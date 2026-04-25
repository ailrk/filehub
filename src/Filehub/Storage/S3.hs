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
module Filehub.Storage.S3 (storage) where

import Data.ClientPath (fromClientPath)
import Filehub.Error
import Filehub.Session.Types (TargetView(..))
import Filehub.Types (SessionId)
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)
import Storage.S3 qualified
import Target.S3 (Target, S3)
import Target.Storage (Storage(..))
import Target.Types (handleTarget, targetHandler)
import Filehub.Session.Effectful qualified as Session
import Filehub.Monad (Filehub)
import UnliftIO (throwIO)


storage :: SessionId -> Storage Filehub
storage sessionId =
  Storage
    { get = \path -> do
        s3 <- getS3 sessionId
        Storage.S3.get s3 path

    , read = \file -> do
        s3 <- getS3 sessionId
        Storage.S3.read s3 file

    , readStream = \file -> do
        s3 <- getS3 sessionId
        Storage.S3.readStream s3 file

    , write = \fileWithContent -> do
        s3 <- getS3 sessionId
        Storage.S3.write s3 fileWithContent

    , mv = \mvPairs -> do
        s3 <- getS3 sessionId
        Storage.S3.mv s3 mvPairs

    , rename = \old new -> do
        s3 <- getS3 sessionId
        Storage.S3.rename s3 old new

    , delete = \filePath -> do
        s3 <- getS3 sessionId
        Storage.S3.delete s3 filePath

    , new = \filePath -> do
        s3 <- getS3 sessionId
        Storage.S3.new s3 filePath

    , newFolder = \_ -> pure ()

    , ls = \filePath -> do
        s3 <- getS3 sessionId
        Storage.S3.ls s3 filePath

    , cd = \_ -> pure ()

    , lsCwd = do
        s3 <- getS3 sessionId
        Storage.S3.lsCwd s3

    , upload = \filedata -> do
        s3 <- getS3 sessionId
        Storage.S3.upload s3 filedata

    , download = \clientPath -> do
        root     <- Session.get sessionId (.root)
        s3       <- getS3 sessionId
        let path =  fromClientPath root clientPath
        Storage.S3.download s3 path
    , isDirectory = \filePath -> do
        s3 <- getS3 sessionId
        Storage.S3.isDirectory s3 filePath
    }


getS3 :: SessionId -> Filehub (Target S3)
getS3 sessionId = do
  TargetView target _ <- Session.get sessionId (.currentTarget)
  maybe (throwIO (FilehubError TargetError "Target is not valid S3 bucket")) pure $ handleTarget target
    [ targetHandler @S3 id
    ]
