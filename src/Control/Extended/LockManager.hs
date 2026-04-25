{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Control.Extended.LockManager
  ( MonadLockManager(..)
  , mkLockKey
  )
  where


import LockRegistry.Key (LockKey, mkLockKey)


class (Monad m) => MonadLockManager m where
  withLock :: LockKey -> m a -> m a
  withLocks :: [LockKey] -> m a -> m a
