{-# LANGUAGE DefaultSignatures #-}
module Effectful.Extended.Cache
  ( MonadCache(..)
  , InMemoryCacheT(..)
  , DummyCacheT(..)
  , runCacheInMemory
  , runCacheDummy
  , mkCacheKey
  )
  where

import Cache.Dummy qualified as Dummy
import Cache.InMemory qualified as InMemory
import Cache.Key (CacheKey, mkCacheKey, SomeCacheKey (..))
import Control.Monad (void)
import Data.Dynamic (Typeable)
import Data.Time (NominalDiffTime, getCurrentTime)
import Prelude hiding (lookup)
import UnliftIO (atomicModifyIORef', readIORef, MonadUnliftIO)
import Control.Monad.Reader (ReaderT (..), MonadReader (..), MonadIO (..))
import Cache.InMemory (InMemoryCache(..))
import Control.Monad.Base (MonadBase)


class Monad m => MonadCache m where
  cacheLookup :: (Typeable a) => CacheKey a -> m (Maybe a)
  cacheInsert :: (Typeable a) => CacheKey a -> [SomeCacheKey] -> Maybe NominalDiffTime -> a -> m ()
  cacheDelete :: SomeCacheKey -> m ()
  cacheFlush :: m ()


newtype InMemoryCacheT m a = InMemoryCacheT { unInMemoryCacheT :: ReaderT InMemoryCache m a }
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadUnliftIO
  , MonadBase b
  , MonadReader InMemoryCache
  )


instance MonadIO m => MonadCache (InMemoryCacheT m) where
  cacheLookup key = do
    InMemory.InMemoryCache cacheRef <- ask
    cache <- liftIO $ readIORef cacheRef
    now <- liftIO getCurrentTime
    case InMemory.lookup now key cache of
      Just (value, cache') -> do
        atomicModifyIORef' cacheRef (const (cache', ()))
        pure value
      Nothing -> pure Nothing
  cacheInsert key mDeps mTTL value = do
    InMemoryCache cacheRef <- ask
    now <- liftIO getCurrentTime
    void $ atomicModifyIORef' cacheRef (\cache -> (InMemory.insert now key mDeps mTTL value cache, ()))
  cacheDelete key = do
    InMemoryCache cacheRef <- ask
    void $ atomicModifyIORef' cacheRef (\cache -> (InMemory.delete key cache, ()))
  cacheFlush = do
    InMemoryCache cacheRef <- ask
    void $ atomicModifyIORef' cacheRef (\cache -> (InMemory.empty cache.capacity, ()))


runCacheInMemory :: InMemoryCache -> InMemoryCacheT m a -> m a
runCacheInMemory cache (InMemoryCacheT m) = (`runReaderT` cache) m


newtype DummyCacheT m a = DummyCacheT { unInMemoryCacheT :: m a }
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadUnliftIO
  , MonadBase b
  )


runCacheDummy :: DummyCacheT m a -> m a
runCacheDummy (DummyCacheT m) = m


instance MonadIO m => MonadCache (DummyCacheT m) where
  cacheLookup = liftIO . Dummy.lookup
  cacheInsert key mDeps mTTL value = liftIO $ Dummy.insert key mDeps mTTL value
  cacheDelete = liftIO . Dummy.delete
  cacheFlush = liftIO $ Dummy.flush
