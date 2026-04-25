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
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Session.Types (TargetView(..))
import Filehub.Session.Effectful (SessionGet(..), SessionSet(..))
import Filehub.Session.Effectful qualified as Session
import Filehub.Types (SessionId)
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)
import Storage.File qualified
import Target.File (Target, FileSys)
import Target.Storage (Storage(..))
import Target.Types (handleTarget, targetHandler)
import Data.ClientPath (AbsPath(..))
import Data.Coerce (coerce)
import Filehub.Monad (Filehub)
import UnliftIO.Directory (doesDirectoryExist)
import Log (logAttention)
import UnliftIO (throwIO)


cd :: SessionId -> AbsPath -> Filehub ()
cd sessionId dir = do
  exists <- doesDirectoryExist (coerce dir)
  unless exists do
    logAttention "[nmb224] dir doesn't exists:" dir
    throwIO (FilehubError InvalidDir "Can't enter, not a directory")
  Session.set (.currentDir) dir


storage :: SessionId -> Storage Filehub
storage sessionId =
  Storage
    { get         = Storage.File.get
    , read        = Storage.File.read
    , readStream  = Storage.File.readStream
    , ls          = Storage.File.ls
    , cd          = cd sessionId
    , isDirectory = Storage.File.isDirectory

    , write = \fileWithContent -> do
        Storage.File.write fileWithContent

    , mv = \mvPairs -> do
        Storage.File.mv mvPairs

    , rename = \old new -> do
        Storage.File.rename old new

    , delete = \path-> do
        Storage.File.delete path

    , new = \path -> do
        Storage.File.new path

    , newFolder = \path -> do
        Storage.File.newFolder path

    , lsCwd = do
        currentDir <- Session.get (.currentDir)
        Storage.File.lsCwd currentDir

    , upload = \filedata -> do
        currentDir <- Session.get (.currentDir)
        Storage.File.upload currentDir filedata

    , download = \clientPath -> do
        fileSys <- getFileSys sessionId
        Storage.File.download fileSys clientPath
    }



getFileSys :: SessionId -> Filehub (Target FileSys)
getFileSys sessionId = do
  TargetView target _ <- Session.get (.currentTarget)
  maybe (throwIO (FilehubError TargetError "Target is not valid file system direcotry")) pure $ handleTarget target
    [ targetHandler @FileSys id
    ]
