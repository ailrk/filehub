-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- An in memory LRU cache based on priority search queue.
module Filehub.Cache.InMemory where

import Filehub.Cache.Key (CacheKey)
import Data.Dynamic (Dynamic, fromDynamic, Typeable, toDyn)
import Prelude hiding (lookup)
import Data.HashPSQ qualified as HashPSQ
import Data.HashPSQ (HashPSQ)
import Data.Bifunctor (Bifunctor(..))
import UnliftIO (IORef)
import Data.IORef (newIORef)


-- | Priority is a monotonically increasing tick. Every insertion or lookup
-- will bump the tick.
type Priority = Integer


newtype InMemoryCache = InMemoryCache (IORef Cache)


new ::  Int -> IO InMemoryCache
new capacity = do
  ref <- newIORef (empty capacity)
  pure (InMemoryCache ref)


data Cache = Cache
  { capacity :: Int
  , size     :: Int
  , tick     :: Priority
  , queue    :: HashPSQ CacheKey Priority Dynamic
  }


empty :: Int -> Cache
empty capacity
  | capacity < 1 = error "cache capacity < 1"
  | otherwise = Cache
    { capacity = capacity
    , size     = 0
    , tick     = 0
    , queue    = HashPSQ.empty
    }


-- | Ensure the queue invariant
trim :: Cache -> Cache
trim cache
  | cache.size > cache.capacity = cache
    { size  = cache.size - 1
    , queue = HashPSQ.deleteMin cache.queue
    }
  | otherwise = cache


insert :: forall a . Typeable a => CacheKey -> a -> Cache -> Cache
insert key value = insertDyn key (toDyn value)


insertDyn :: CacheKey -> Dynamic -> Cache -> Cache
insertDyn key val cache = trim cache'
  where
    (mEvicted, queue) = HashPSQ.insertView key cache.tick val cache.queue
    cache' = cache
      { size  = maybe (cache.size + 1) (const cache.size) mEvicted
      , tick  = cache.tick + 1
      , queue = queue
      }


lookup :: forall a . (Typeable a) => CacheKey -> Cache -> Maybe (Maybe a, Cache)
lookup key cache = fmap (bimap (fromDynamic @a) id) (lookupDyn key cache)


lookupDyn :: CacheKey -> Cache -> Maybe (Dynamic, Cache)
lookupDyn key cache =
  case HashPSQ.alter lookupAndBump key cache.queue of
    (Nothing, _) -> Nothing
    (Just x, queue') -> Just (x, trim $ cache { tick = cache.tick + 1, queue = queue'})
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x,  Just (cache.tick, x))


delete :: CacheKey -> Cache -> Cache
delete key cache = cache { queue = HashPSQ.delete key cache.queue }
