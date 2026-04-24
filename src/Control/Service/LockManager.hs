{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Control.Service.LockManager
  ( MonadLockManager(..)
  , mkLockKey
  )
  where


import LockManager.Key (LockKey, mkLockKey)


class (Monad m) => MonadLockManager m where
  withLock :: LockKey -> m a -> m a
  withLocks :: [LockKey] -> m a -> m a
