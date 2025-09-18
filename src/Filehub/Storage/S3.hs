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

import Data.Generics.Labels ()
import Data.Generics.Labels ()
import Effectful (Eff, Eff, (:>), IOE)
import Filehub.Types (SessionId, Env)
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)
import Filehub.Storage.Types (Storage(..))
import Storage.S3 qualified
import Filehub.Session.Types (TargetView(..))
import Target.S3 (Backend, S3)
import {-# SOURCE #-} Filehub.Session qualified as Session
import Effectful.Error.Dynamic (throwError, Error)
import Filehub.Error
import Target.Types (handleTarget, targetHandler)
import Data.ClientPath (fromClientPath)
import Effectful.Reader.Dynamic
import Effectful.Log (Log)
import Effectful.Extended.Cache (Cache)


storage
  :: ( Reader Env         :> es
     , Log                :> es
     , IOE                :> es
     , Cache              :> es
     , Error FilehubError :> es
     )
  => SessionId -> Storage (Eff es)
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

    , write = \filePath bytes -> do
        s3 <- getS3 sessionId
        Storage.S3.write s3 filePath bytes

    , writeStream = \filePath conduit -> do
        s3 <- getS3 sessionId
        Storage.S3.writeStream s3 filePath conduit

    , cp = \src dst -> do
        s3 <- getS3 sessionId
        Storage.S3.cp s3 src dst

    , delete = \filePath -> do
        s3 <- getS3 sessionId
        Storage.S3.delete s3 filePath

    , new = \filePath -> do
        s3 <- getS3 sessionId
        Storage.S3.new s3 filePath

    , newFolder = \filePath -> do
        s3 <- getS3 sessionId
        Storage.S3.newFolder s3 filePath

    , ls = \filePath -> do
        s3 <- getS3 sessionId
        Storage.S3.ls s3 filePath

    , cd = \_ -> pure ()

    , lsCwd = do
        s3 <- getS3 sessionId
        Storage.S3.lsCwd s3

    , upload = \multipart -> do
        s3 <- getS3 sessionId
        Storage.S3.upload s3 multipart

    , download = \clientPath -> do
        root     <- Session.getRoot sessionId
        s3       <- getS3 sessionId
        let path =  fromClientPath root clientPath
        Storage.S3.download s3 path

    , isDirectory = \filePath -> do
        s3 <- getS3 sessionId
        Storage.S3.isDirectory s3 filePath
    }


getS3
  :: ( Reader Env         :> es
     , Log                :> es
     , IOE                :> es
     , Error FilehubError :> es)
  => SessionId -> Eff es (Backend S3)
getS3 sessionId = do
  TargetView target _ _ <- Session.currentTarget sessionId
  maybe (throwError (FilehubError TargetError "Target is not valid S3 bucket")) pure $ handleTarget target
    [ targetHandler @S3 id
    ]
