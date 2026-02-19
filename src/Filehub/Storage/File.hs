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
import Data.Function ((&))
import Effectful ( raise)
import Effectful.Error.Dynamic (throwError)
import Effectful.FileSystem
import Effectful.Log
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Monad (Filehub)
import Filehub.Session.Types (TargetView(..))
import Filehub.Storage.Error (withStorageError)
import Filehub.Types ( SessionId )
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)
import Storage.File qualified
import Target.File (TargetBackend, FileSys)
import Target.Storage (Storage(..))
import Target.Types (handleTarget, targetHandler)
import {-# SOURCE #-} Filehub.Session qualified as Session
import Data.ClientPath (AbsPath(..))
import Data.Coerce (coerce)


cd :: SessionId -> AbsPath -> Filehub ()
cd sessionId dir = do
  exists <- doesDirectoryExist (coerce dir)
  unless exists do
    logAttention "[nmb224] dir doesn't exists:" dir
    throwError (FilehubError InvalidDir "Can't enter, not a directory")
  Session.setCurrentDir sessionId dir


storage :: SessionId -> Storage Filehub FileSys
storage sessionId =
  Storage
    { get         = Storage.File.get
    , read        = Storage.File.read
    , readStream  = Storage.File.readStream
    , ls          = withStorageError . Storage.File.ls
    , cd          = cd sessionId
    , isDirectory = Storage.File.isDirectory

    , write = \fileWithContent -> do
        Storage.File.write fileWithContent

    , mv = \mvPairs -> withStorageError do
        Storage.File.mv mvPairs

    , rename = \old new -> withStorageError do
        Storage.File.rename old new

    , delete = \path-> do
        Storage.File.delete path

    , new = \path -> withStorageError do
        Storage.File.new path

    , newFolder = \path -> withStorageError do
        Storage.File.newFolder path

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
