{-# LANGUAGE RecordWildCards #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- An in memory LRU cache based on priority search queue.
module Cache.InMemory where

import Cache.Key (CacheKey (..), SomeCacheKey(..))
import Data.Dynamic (Dynamic, fromDynamic, Typeable, toDyn)
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as HashPSQ
import Data.IORef (newIORef)
import Prelude hiding (lookup)
import UnliftIO (IORef)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Function ((&))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)


-- | Priority is a monotonically increasing tick. Every insertion or lookup
-- will bump the tick.
type Priority = Integer


newtype InMemoryCache = InMemoryCache (IORef Cache)


-- | A single cache entry
data Entry = Entry
  { val      :: Dynamic
  , expiryAt :: Maybe UTCTime
  }
  deriving Show


-- | Cache key dependency graph. When updating/deleting a cache key, we will
-- also delete all of its dependencies.
newtype CacheDependencies = CacheDependencies (Map SomeCacheKey (Set SomeCacheKey))
  deriving Show


-- | DFS to get the set of all reachable cache keys.
reachable :: SomeCacheKey -> Map SomeCacheKey (Set SomeCacheKey) -> Set SomeCacheKey
reachable key graph = go mempty [key]
  where
    go seen [] = seen
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise =
          let neighbors = Map.findWithDefault mempty x graph
           in go (Set.insert x seen) (Set.toList neighbors ++ xs)


new ::  Int -> IO InMemoryCache
new capacity = do
  ref <- newIORef (empty capacity)
  pure (InMemoryCache ref)


data Cache = Cache
  { capacity     :: Int
  , size         :: Int
  , tick         :: Priority
  , queue        :: HashPSQ SomeCacheKey Priority Entry
  , dependencies :: CacheDependencies
  }


empty :: Int -> Cache
empty capacity
  | capacity < 1 = error "cache capacity < 1"
  | otherwise = Cache
    { capacity     = capacity
    , size         = 0
    , tick         = 0
    , queue        = HashPSQ.empty
    , dependencies = CacheDependencies mempty
    }


-- | Ensure the queue invariant
trim :: Cache -> Cache
trim cache
  | cache.size > cache.capacity = cache
    { size  = cache.size - 1
    , queue = HashPSQ.deleteMin cache.queue
    }
  | otherwise = cache


-- | Insert a new data to the cache
insert :: forall a . Typeable a
       => UTCTime
      -- ^ The insertion time
       -> CacheKey a
       -- ^ The cache key to insert
       -> [SomeCacheKey]
       -- ^ Keys that this cache key depends on
       -> Maybe NominalDiffTime
       -- ^ The Cache TTL. Nothing indicates that the cache lives forever
       -> a
       -- ^ The cache to be inserted. This will be stoed as a `Dynamic`.
       -> Cache -> Cache
insert now key deps mTTL value = insertDyn now (SomeCacheKey key) deps mTTL (toDyn value)


-- | Insert a new `Dynamic` to the cache. If the cache is out of space, we evict the least used element.
-- That is, the elment with the smallest priority.
insertDyn :: UTCTime
          -- ^ The insertion time
          -> SomeCacheKey
          -- ^ The cache key to insert
          -> [SomeCacheKey]
          -- ^ Keys that this cache key depends on
          -> Maybe NominalDiffTime
          -- ^ The Cache TTL. Nothing indicates that the cache lives forever
          -> Dynamic
          -- ^ The cache is represented as `Dynamic`
          -> Cache -> Cache
insertDyn now key deps mTTL value cache
  = cache
    { queue = queue
    , tick  = cache.tick + 1
    , size  = maybe (cache.size + 1) (const cache.size) mEvicted
    }
  & updateDependencies
  & trim
  where
    entry = Entry value (fmap (`addUTCTime` now) mTTL)
    (mEvicted, queue) = HashPSQ.insertView key cache.tick entry cache.queue
    updateDependencies c =
      case deps of
        []   -> c
        _ ->
          c { dependencies =
                let cacheDeps = coerce @_ @(Map SomeCacheKey (Set SomeCacheKey)) c.dependencies
                 in coerce $
                   foldr
                    (Map.alter (Just . (Set.insert key) . fromMaybe mempty))
                    cacheDeps deps
            }


-- | Lookup a cached data with `CacheKey a`
lookup :: forall a . (Typeable a) => UTCTime -> CacheKey a -> Cache -> Maybe (Maybe a, Cache)
lookup now key cache =
  case lookupDyn now (SomeCacheKey key) cache of
    Just (Nothing, cache') -> Just (Nothing, cache')
    Just (Just d, cache')  -> Just (fromDynamic @a d, cache')
    Nothing                -> Nothing


-- | Lookup a cached dynamic data. If the key is expired, we simply delete the key from the cache.
-- Otherwise we bump the priority of the cached data so it's at the top.
lookupDyn :: UTCTime -> SomeCacheKey -> Cache -> Maybe (Maybe Dynamic, Cache)
lookupDyn now key cache =
  case HashPSQ.alter lookupAndBump key cache.queue of
    (Nothing, _) -> Nothing
    (Just (Entry val Nothing), queue') ->
      Just (Just val, trim $ cache { tick = cache.tick + 1, queue = queue'})
    (Just (Entry val (Just expiryAt)), queue')
      | now < expiryAt -> do
        Just (Just val, trim $ cache { tick = cache.tick + 1, queue = queue'})
      | otherwise -> do
        Just (Nothing, delete key cache)
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x,  Just (cache.tick, x))


-- | Delete a cache key from the cache. If there are other keys depend on it, we delete all of them.
delete :: SomeCacheKey -> Cache -> Cache
delete key cache@Cache { dependencies = dependencies } =
  cache
    { queue = foldr HashPSQ.delete cache.queue toDelete
    , dependencies = coerce
                   $ fmap (`Set.difference` toDelete)
                   $ foldr Map.delete (coerce dependencies)
                   $ toDelete
    }
  where
    toDelete = reachable key (coerce dependencies)
