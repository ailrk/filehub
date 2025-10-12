{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Control.Monad (unless)
import Effectful ( raise)
import Effectful.Error.Dynamic (throwError)
import Effectful.FileSystem
import Effectful.Log
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Monad (Filehub)
import Filehub.Storage.Error (withStorageError)
import Filehub.Storage.Types (Storage (..))
import Filehub.Types ( SessionId )
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)
import Storage.File qualified
import {-# SOURCE #-} Filehub.Session qualified as Session
import Data.Function ((&))
import Target.File (TargetBackend, FileSys)
import Filehub.Session.Types (TargetView(..))
import Target.Types (handleTarget, targetHandler)


cd :: SessionId -> FilePath -> Filehub ()
cd sessionId path = do
  exists <- doesDirectoryExist path
  unless exists do
    logAttention "[nmb224] dir doesn't exists:" path
    throwError (FilehubError InvalidDir "Can enter, not a directory")
  Session.setCurrentDir sessionId path



storage :: SessionId -> Storage Filehub
storage sessionId =
  Storage
    { get         = Storage.File.get
    , read        = Storage.File.read
    , readStream  = Storage.File.readStream
    , ls          = withStorageError . Storage.File.ls
    , cd          = cd sessionId
    , isDirectory = Storage.File.isDirectory

    , write = \name fileWithContent -> do
        currentDir <- Session.getCurrentDir sessionId
        Storage.File.write currentDir name fileWithContent

    , mv = \mvPairs -> withStorageError do
        currentDir <- Session.getCurrentDir sessionId & raise
        Storage.File.mv currentDir mvPairs

    , delete = \name -> do
        currentDir <- Session.getCurrentDir sessionId
        Storage.File.delete currentDir name

    , new = \name -> withStorageError do
        currentDir <- Session.getCurrentDir sessionId & raise
        Storage.File.new currentDir name

    , newFolder = \name -> withStorageError do
        currentDir <- Session.getCurrentDir sessionId & raise
        Storage.File.newFolder currentDir name

    , lsCwd = withStorageError do
        currentDir <- Session.getCurrentDir sessionId & raise
        Storage.File.lsCwd currentDir

    , upload = \filedata -> do
        currentDir <- Session.getCurrentDir sessionId
        Storage.File.upload currentDir filedata

    , download = \clientPath -> do
        fileSys <- getFileSys sessionId
        Storage.File.download fileSys clientPath
    }



getFileSys :: SessionId -> Filehub (TargetBackend FileSys)
getFileSys sessionId = do
  TargetView target _ <- Session.currentTarget sessionId
  maybe (throwError (FilehubError TargetError "Target is not valid file system direcotry")) pure $ handleTarget target
    [ targetHandler @FileSys id
    ]
