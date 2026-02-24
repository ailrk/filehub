-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the `LockRegistry` type that manages locks in the system.
-- The `LockRegistry` maps a `LockKey` to a `TMVar ()`, which is used to control the
-- synchronization among threads. the `LockKey` can be derived from any data type and
-- should be unique to that data type.
--
-- This `LockRegistry` works in a single local instance. For distributed lock, check
-- `Filehub.LockRegistry.Remote`
module LockRegistry.Local
  ( new
  , withLock
  , withLocks
  , LockRegistry(..)
  )
  where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified  as Map
import LockRegistry.Key (LockKey)
import UnliftIO (finally)
import UnliftIO.STM (TVar, TMVar, newTVarIO, atomically, readTVar, newTMVar, writeTVar, takeTMVar, putTMVar)
import Data.List (nub, sort)


-- | A global lock registry
newtype LockRegistry = LockRegistry (TVar (Map LockKey (TMVar ())))


new :: IO LockRegistry
new = LockRegistry <$> newTVarIO Map.empty


withLock :: LockRegistry -> LockKey -> IO a -> IO a
withLock (LockRegistry tv) key action = do
  lock <- atomically do
    m <- readTVar tv
    case Map.lookup key m of
      Just tmv -> return tmv
      Nothing -> do
        tmv <- newTMVar ()
        writeTVar tv (Map.insert key tmv m)
        pure tmv
  atomically $ takeTMVar lock
  result <- action `finally` (atomically $ putTMVar lock ())
  pure result


-- | Lock with a list of keys. Keys are deduplicated and sorted to avoid
-- circular deadlock.
withLocks :: LockRegistry -> [LockKey] -> IO a -> IO a
withLocks lr keys action = go (sort (nub keys))
  where
    go []     = action
    go (k:ks) = withLock lr k (go ks)
