{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Effectful.Extended.LockManager
  ( LockManager
  , runLockManagerLocal
  , runLockManagerDummy
  , withLock
  , mkLockKey
  )
  where


import Effectful
import Effectful.Dispatch.Dynamic
import LockRegistry.Local qualified as Local
import LockRegistry.Key (LockKey, mkLockKey)
import LockRegistry.Dummy qualified as Dummy


data LockManager :: Effect where
  WithLock :: LockKey -> m a -> LockManager m a


type instance DispatchOf LockManager = Dynamic


runLockManagerLocal :: (IOE :> es) => Local.LockRegistry -> Eff (LockManager : es) a -> Eff es a
runLockManagerLocal registry = reinterpret id \env -> \case
  WithLock key action -> do
    localSeqUnliftIO env \toIO -> do
      Local.withLock registry key (toIO action)


runLockManagerDummy :: (IOE :> es) => Eff (LockManager : es) a -> Eff es a
runLockManagerDummy = reinterpret id \env -> \case
  WithLock key action -> do
    localSeqUnliftIO env \toIO -> do
      Dummy.withLock key (toIO action)


withLock :: (LockManager :> es) => LockKey -> Eff es a -> Eff es a
withLock key action = send (WithLock key action)
