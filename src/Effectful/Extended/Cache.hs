module Effectful.Extended.Cache
  ( Cache(..)
  , runCacheInMemory
  , runCacheDummy
  , lookup
  , insert
  , delete
  , flush
  , mkCacheKey
  )
  where

import Cache.Dummy qualified as Dummy
import Cache.InMemory qualified as InMemory
import Cache.Key (CacheKey, mkCacheKey, SomeCacheKey (..))
import Control.Monad (void)
import Data.Dynamic (Typeable)
import Data.String.Interpolate (i)
import Data.Time (NominalDiffTime, getCurrentTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Log (logTrace_, Log)
import Prelude hiding (lookup)
import UnliftIO (atomicModifyIORef', readIORef)


-- | A generic cache effect
data Cache :: Effect where
  Lookup :: (Typeable a) => CacheKey a -> Cache m (Maybe a)
  Insert :: (Typeable a) => CacheKey a -> [SomeCacheKey] -> Maybe NominalDiffTime -> a -> Cache m ()
  Delete :: SomeCacheKey -> Cache m ()
  Flush  :: Cache m ()


type instance DispatchOf Cache = Dynamic


runCacheInMemory :: (IOE :> es) => InMemory.InMemoryCache -> Eff (Cache : es) a -> Eff es a
runCacheInMemory (InMemory.InMemoryCache cacheRef) = interpret \_ -> \case
  Lookup key -> do
    cache <- readIORef cacheRef
    now <- liftIO getCurrentTime
    case InMemory.lookup now key cache of
      Just (value, cache') -> do
        atomicModifyIORef' cacheRef (const (cache', ()))
        pure value
      Nothing -> pure Nothing
  Insert key mDeps mTTL value -> do
      now <- liftIO getCurrentTime
      void $ atomicModifyIORef' cacheRef (\cache -> (InMemory.insert now key mDeps mTTL value cache, ()))
  Delete key -> do
      void $ atomicModifyIORef' cacheRef (\cache -> (InMemory.delete key cache, ()))
  Flush ->
      void $ atomicModifyIORef' cacheRef (\cache -> (InMemory.empty cache.capacity, ()))


runCacheDummy :: (IOE :> es) =>  Eff (Cache : es) a -> Eff es a
runCacheDummy = interpret \_ -> \case
  Lookup key                  -> liftIO $ Dummy.lookup key
  Insert key mDeps mTTL value -> liftIO $ Dummy.insert key mDeps mTTL value
  Delete key                  -> liftIO $ Dummy.delete key
  Flush                       -> liftIO Dummy.flush


lookup :: forall a es . (Cache :> es, Log :> es, Typeable a) => CacheKey a -> Eff es (Maybe a)
lookup key = do
  result <- send (Lookup key)
  case result of
    Just _ -> logTrace_ [i|[9vd2zj] CACHE HIT #{key}|]
    Nothing -> pure ()
  pure result


insert :: forall a es . (Cache :> es, Typeable a)
       => CacheKey a -> [SomeCacheKey] -> Maybe NominalDiffTime -> a -> Eff es ()
insert key deps mTTL value = send (Insert key deps mTTL value)


delete :: (Cache :> es, Log :> es) => CacheKey a -> Eff es ()
delete key = do
  logTrace_ [i|[0cvba1] CACHE DELETE #{key}|]
  send (Delete (SomeCacheKey key))


flush :: (Cache :> es) => Eff es ()
flush = send Flush
