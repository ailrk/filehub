module Control.Handle.Cache where

import Cache.InMemory (InMemoryCache(..))
import Cache.InMemory qualified as InMemory
import Cache.Key (CacheKey, SomeCacheKey)
import Control.Monad (void)
import Data.Data (Typeable)
import Data.Time (NominalDiffTime, getCurrentTime)
import UnliftIO (MonadIO (..), readIORef, atomicModifyIORef')


data Cache m = Cache
  { lookup :: forall a . (Typeable a) => CacheKey a -> m (Maybe a)
  , insert :: forall a . (Typeable a) => CacheKey a -> [SomeCacheKey] -> Maybe NominalDiffTime -> a -> m ()
  , delete :: SomeCacheKey -> m ()
  , flush  :: m ()
  }


makeInMemoryCache :: MonadIO m => InMemoryCache -> Cache m
makeInMemoryCache (InMemoryCache ref) =
  Cache
    { lookup = \key -> do
        cache <- liftIO (readIORef ref)
        now <- liftIO getCurrentTime
        case InMemory.lookup now key cache of
          Just (value, cache') -> do
            atomicModifyIORef' ref (const (cache', ()))
            pure value
          Nothing -> pure Nothing

    , insert = \key mDeps mTTL value -> do
        now <- liftIO getCurrentTime
        void do
          atomicModifyIORef' ref
            \cache -> (InMemory.insert now key mDeps mTTL value cache, ())

    , delete = \key -> do
        void do
          atomicModifyIORef' ref
            \cache -> (InMemory.delete key cache, ())

    , flush  = void do
          atomicModifyIORef' ref
            \cache -> (InMemory.empty cache.capacity, ())
    }
{-# INLINABLE makeInMemoryCache #-}
