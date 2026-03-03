{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the `LockRegistry` type that manages locks in the system.
-- The `LockRegistry` contains a vector oo
--
--
--
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
import Data.Vector qualified as Vector
import Data.Vector (Vector)
import LockRegistry.Key (LockKey)
import UnliftIO (finally, MVar, bracket, withMVar, newMVar)
import UnliftIO.STM (TVar, TMVar, newTVarIO, atomically, readTVar, newTMVar, writeTVar, takeTMVar, putTMVar)
import Data.List (nub, sort)
import Data.Hashable (Hashable(..))
import Data.Bits ((.&.))


newtype Shard = Shard (TVar (Map LockKey LockEntry))


data LockEntry = LockEntry
  { lk       :: !(MVar ())
  , refCount :: !Int
  }


data LockRegistry = LockRegistry
  { shards :: !(Vector Shard)
  , mask   :: !Int
  }


newShards :: Int -> IO LockRegistry
newShards n = do
  let nShards = 2 ^ n
  shards <- Vector.replicateM nShards (Shard <$> newTVarIO Map.empty)
  pure LockRegistry { shards = shards, mask = nShards - 1 }


new :: IO LockRegistry
new = newShards 10


getShard :: LockRegistry -> LockKey -> Shard
getShard LockRegistry{ shards, mask } key =
  Vector.unsafeIndex shards (hash  key .&. mask)


withLock :: LockRegistry -> LockKey -> IO a -> IO a
withLock registry key action = do
  let shard = getShard registry key
  lk <- newMVar ()
  bracket (acquire lk shard) (release shard) use
  where
    acquire lk (Shard shard) = atomically do
      m <- readTVar shard
      case  Map.lookup key m of
        Just entry -> do
          let !newEntry = entry { refCount = entry.refCount + 1 }
          writeTVar shard (Map.insert key newEntry m)
          pure (entry.lk)
        Nothing -> do
          let !newEntry = LockEntry lk 1
          writeTVar shard (Map.insert key newEntry m)
          pure lk
    release (Shard shard) _ = atomically do
      m <- readTVar shard
      case Map.lookup key m of
        Just entry -> do
          if entry.refCount <= 1
             then writeTVar shard (Map.delete key m)
             else let !newEntry = entry { refCount = entry.refCount - 1 }
                   in writeTVar shard (Map.insert key newEntry m)
        Nothing -> pure ()
    use lk = withMVar lk (const action)


-- | Lock with a list of keys. Keys are deduplicated and sorted to avoid
-- circular deadlock.
withLocks :: LockRegistry -> [LockKey] -> IO a -> IO a
withLocks lr keys action = go (sort (nub keys))
  where
    go []     = action
    go (k:ks) = withLock lr k (go ks)
