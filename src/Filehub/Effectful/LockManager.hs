{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Filehub.Effectful.LockManager
  ( LockManager
  , runLockManagerLocal
  , withLock
  , mkLockKey
  )
  where


import Effectful
import Effectful.Dispatch.Dynamic
import Filehub.LockRegistry.Local qualified as Local
import Filehub.LockRegistry.Key (LockKey, mkLockKey)


data LockManager :: Effect where
  WithLock :: LockKey -> m a -> LockManager m a


type instance DispatchOf LockManager = Dynamic


runLockManagerLocal :: (IOE :> es) => Local.LockRegistry -> Eff (LockManager : es) a -> Eff es a
runLockManagerLocal registry = reinterpret id \env -> \case
  WithLock key action -> do
    localSeqUnliftIO env \toIO -> do
      Local.withLock registry key (toIO action)

withLock :: (LockManager :> es) => LockKey -> Eff es a -> Eff es a
withLock key action = send (WithLock key action)
