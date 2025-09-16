-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- An in memory LRU cache based on priority search queue.
module Cache.InMemory where

import Cache.Key (CacheKey)
import Data.Dynamic (Dynamic, fromDynamic, Typeable, toDyn)
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as HashPSQ
import Data.IORef (newIORef)
import Prelude hiding (lookup)
import UnliftIO (IORef)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)


-- | Priority is a monotonically increasing tick. Every insertion or lookup
-- will bump the tick.
type Priority = Integer


newtype InMemoryCache = InMemoryCache (IORef Cache)


data Entry = Entry
  { val      :: Dynamic
  , expiryAt :: Maybe UTCTime
  }


new ::  Int -> IO InMemoryCache
new capacity = do
  ref <- newIORef (empty capacity)
  pure (InMemoryCache ref)


data Cache = Cache
  { capacity :: Int
  , size     :: Int
  , tick     :: Priority
  , queue    :: HashPSQ CacheKey Priority Entry
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


insert :: forall a . Typeable a
       => UTCTime
       -> CacheKey
       -> Maybe NominalDiffTime
       -> a
       -> Cache -> Cache
insert now key mTTL value = insertDyn now key mTTL (toDyn value)


insertDyn :: UTCTime
          -> CacheKey
          -> Maybe NominalDiffTime
          -> Dynamic
          -> Cache -> Cache
insertDyn now key mTTL value cache = trim cache'
  where
    entry = Entry value (fmap (`addUTCTime` now) mTTL)
    (mEvicted, queue) = HashPSQ.insertView key cache.tick entry cache.queue
    cache' = cache
      { size  = maybe (cache.size + 1) (const cache.size) mEvicted
      , tick  = cache.tick + 1
      , queue = queue
      }


lookup :: forall a . (Typeable a) => UTCTime -> CacheKey -> Cache -> Maybe (Maybe a, Cache)
lookup now key cache =
  case lookupDyn now key cache of
    Just (Nothing, cache') -> Just (Nothing, cache')
    Just (Just d, cache')  -> Just (fromDynamic @a d, cache')
    Nothing                -> Nothing


lookupDyn :: UTCTime -> CacheKey -> Cache -> Maybe (Maybe Dynamic, Cache)
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


delete :: CacheKey -> Cache -> Cache
delete key cache = cache { queue = HashPSQ.delete key cache.queue }
