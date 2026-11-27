module Control.Handle.Cache where

import Cache.InMemory (InMemoryCache(..))
import Cache.InMemory qualified as InMemory
import Cache.Key (CacheKey, SomeCacheKey)
import Control.Monad (void)
import Data.Data (Typeable)
import Data.IORef (readIORef, atomicModifyIORef')
import Data.Time.Clock (getCurrentTime, NominalDiffTime)
import UnliftIO (MonadIO, liftIO)


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
            liftIO $ atomicModifyIORef' ref (const (cache', ()))
            pure value
          Nothing -> pure Nothing

    , insert = \key mDeps mTTL value -> do
        now <- liftIO getCurrentTime
        liftIO $ void $
          atomicModifyIORef' ref
            \cache -> (InMemory.insert now key mDeps mTTL value cache, ())

    , delete = \key ->
        liftIO $ void $
          atomicModifyIORef' ref
            \cache -> (InMemory.delete key cache, ())

    , flush  =
        liftIO $ void $
          atomicModifyIORef' ref
            \cache -> (InMemory.empty cache.capacity, ())
    }
{-# INLINABLE makeInMemoryCache #-}
