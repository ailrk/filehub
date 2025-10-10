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
import Data.ClientPath (fromClientPath)
import Effectful ( Eff, Eff, (:>), IOE)
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.Extended.Cache (Cache)
import Effectful.Extended.LockManager (LockManager)
import Effectful.FileSystem
import Effectful.Log
import Effectful.Reader.Dynamic (Reader)
import Effectful.Temporary (Temporary)
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Storage.Error (withStorageError)
import Filehub.Storage.Types (Storage (..))
import Filehub.Types ( SessionId, Env )
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)
import Storage.File qualified
import {-# SOURCE #-} Filehub.Session qualified as Session
import Effectful.Concurrent (Concurrent)


cd
  ::
     ( Reader Env         :> es
     , FileSystem         :> es
     , Log                :> es
     , IOE                :> es
     , Error FilehubError :> es)
  => SessionId -> FilePath -> Eff es ()
cd sessionId path = do
  exists <- doesDirectoryExist path
  unless exists do
    logAttention "[cd] dir doesn't exists:" path
    throwError (FilehubError InvalidDir "Can enter, not a directory")
  Session.setCurrentDir sessionId path



storage
  :: ( Reader Env         :> es
     , FileSystem         :> es
     , Temporary          :> es
     , Log                :> es
     , IOE                :> es
     , Cache              :> es
     , LockManager        :> es
     , Concurrent         :> es
     , Error FilehubError :> es
     )
  => SessionId -> (Storage (Eff es))
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
        currentDir <- Session.getCurrentDir sessionId
        Storage.File.mv currentDir mvPairs

    , delete = \name -> do
        currentDir <- Session.getCurrentDir sessionId
        Storage.File.delete currentDir name

    , new = \name -> withStorageError do
        currentDir <- Session.getCurrentDir sessionId
        Storage.File.new currentDir name

    , newFolder = \name -> withStorageError do
        currentDir <- Session.getCurrentDir sessionId
        Storage.File.newFolder currentDir name

    , lsCwd = withStorageError do
        currentDir <- Session.getCurrentDir sessionId
        Storage.File.lsCwd currentDir

    , upload = \filedata -> do
        currentDir <- Session.getCurrentDir sessionId
        Storage.File.upload currentDir filedata

    , download = \clientPath -> do
        root <- Session.getRoot sessionId
        let path =  fromClientPath root clientPath
        Storage.File.download path
    }
