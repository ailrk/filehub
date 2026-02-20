{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Effectful (Eff)
import Effectful.Error.Dynamic (throwError)
import Effectful.FileSystem
import Effectful.Log
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Monad (IsFilehub)
import Filehub.Session.Types (TargetView(..))
import Filehub.Session.Effectful (runSessionEff, SessionGet(..), SessionSet(..))
import Filehub.Session.Effectful qualified as Session
import Filehub.Storage.Error (withStorageError)
import Filehub.Types (SessionId)
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)
import Storage.File qualified
import Target.File (Target, FileSys)
import Target.Storage (Storage(..))
import Target.Types (handleTarget, targetHandler)
import Data.ClientPath (AbsPath(..))
import Data.Coerce (coerce)


cd :: IsFilehub es => SessionId -> AbsPath -> Eff es ()
cd sessionId dir = runSessionEff sessionId do
  exists <- doesDirectoryExist (coerce dir)
  unless exists do
    logAttention "[nmb224] dir doesn't exists:" dir
    throwError (FilehubError InvalidDir "Can't enter, not a directory")
  Session.set (.currentDir) dir


storage :: IsFilehub es => SessionId -> Storage (Eff es)
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
        currentDir <- runSessionEff sessionId $ Session.get (.currentDir)
        Storage.File.lsCwd currentDir

    , upload = \filedata -> do
        currentDir <- runSessionEff sessionId $ Session.get (.currentDir)
        Storage.File.upload currentDir filedata

    , download = \clientPath -> do
        fileSys <- getFileSys sessionId
        Storage.File.download fileSys clientPath
    }



getFileSys :: IsFilehub es => SessionId -> Eff es (Target FileSys)
getFileSys sessionId = runSessionEff sessionId do
  TargetView target _ <- Session.get (.currentTarget)
  maybe (throwError (FilehubError TargetError "Target is not valid file system direcotry")) pure $ handleTarget target
    [ targetHandler @FileSys id
    ]
