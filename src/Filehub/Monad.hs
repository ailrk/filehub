{-# LANGUAGE ConstraintKinds #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- The effect of filehub.
module Filehub.Monad
  ( runFilehub
  , toIO
  , Filehub
  , IsFilehub
  )
  where

import Effectful.Reader.Dynamic
import Effectful (Eff, IOE, runEff, Subset, (:>), Effect)
import Effectful.Log (Log, runLog)
import Effectful.Error.Dynamic (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Concurrent
import Filehub.Env (Env(..))
import Servant (ServerError)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad ((>=>))
import Effectful.Extended.LockManager (LockManager, runLockManagerLocal)
import Effectful.Extended.Cache (Cache, runCacheInMemory)
import Effectful.Temporary (Temporary, runTemporary)
import Filehub.Error (FilehubError, toServerError)
import Data.Kind (Constraint)


type FilehubEffects = [Reader Env, Log, Error FilehubError, FileSystem, Temporary, Concurrent, LockManager, Cache, IOE]


type Filehub = Eff FilehubEffects


type family All (effects :: [Effect]) (es :: [Effect]) :: Constraint where
  All '[] es = ()
  All (eff : effs) es = (eff :> es, All effs es)


type IsFilehub es = All FilehubEffects es


-- | Discharge a `Filehub` effect
runFilehub :: Env -> Filehub a -> IO (Either FilehubError a)
runFilehub env eff =
  runEff $
    runCacheInMemory env.cache
  . runLockManagerLocal env.lockRegistry
  . runConcurrent
  . runTemporary
  . runFileSystem
  . runErrorNoCallStack
  . runLog "filehub" env.logger env.logLevel
  . runReader env
  $ eff


-- | Convenient helper to run Filehub effect in IO.
toIO :: (ServerError -> IO a) -> Env -> Filehub a -> IO a
toIO onErr env eff = do
  (runExceptT >=> either (onErr . toServerError) pure)
  . ExceptT
  . runFilehub env
  $ eff
