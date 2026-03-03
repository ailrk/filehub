{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- `LockRegistry` manages a pool of locks that each corresponds to a `LockKey`.
-- A `Shard` maps a `LockKey` to a `MVar`, A `LockRegistry` contains vector of shards.
-- Lock Maps are sharded to improve the level of concurrency. With sharding, we are less
-- likely to have contention on the same `TVar`. Instead, locking will be spread into
-- different shard.
--
-- Each `LockKey` is hashed into a shard index, and the index is guaranteed to be inside the
-- range of the `Vector`'s indices.
--
-- `LockKey` is unbounded and can grow indefinitely as the program runs. To avoid memory leak,
-- Locks are reference counted. When a lock is not used by anyone, it will be deleted from
-- the registry.
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
import UnliftIO (MVar, bracket, withMVar, newMVar)
import UnliftIO.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
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


-- | Create a new `LockRegistry` with 16 shards.
new :: IO LockRegistry
new = newShards 4


getShard :: LockRegistry -> LockKey -> Shard
getShard LockRegistry{ shards, mask } key =
  Vector.unsafeIndex shards (hash  key .&. mask)


withLock :: LockRegistry -> LockKey -> IO a -> IO a
withLock registry key action = do
  let shard = getShard registry key
  lk <- newMVar ()
  bracket
    (acquire lk shard)
    (release shard)
    use
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
