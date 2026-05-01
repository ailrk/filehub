module Control.Handle.LockManager where

import LockManager.Key (LockKey)
import LockManager.Local qualified as Local
import UnliftIO (MonadUnliftIO (..))


data LockManager m = LockManager
  { withLock :: forall a . LockKey -> m a -> m a
  , withLocks :: forall a . [LockKey] -> m a -> m a
  }


makeLocalLockManager :: MonadUnliftIO m => Local.LockManager -> LockManager m
makeLocalLockManager lkm =
  LockManager
    { withLock = \key action -> do
        withRunInIO \run -> do
          Local.withLock lkm key (run action)

    , withLocks = \keys action -> do
        withRunInIO \run -> do
          Local.withLocks lkm keys (run action)
    }
