{-# LANGUAGE DefaultSignatures #-}
module Control.Extended.Cache
  ( MonadCache(..)
  , mkCacheKey
  )
  where

import Cache.Key (CacheKey, mkCacheKey, SomeCacheKey (..))
import Data.Dynamic (Typeable)
import Data.Time (NominalDiffTime)
import Prelude hiding (lookup)


class Monad m => MonadCache m where
  cacheLookup :: (Typeable a) => CacheKey a -> m (Maybe a)
  cacheInsert :: (Typeable a) => CacheKey a -> [SomeCacheKey] -> Maybe NominalDiffTime -> a -> m ()
  cacheDelete :: SomeCacheKey -> m ()
  cacheFlush :: m ()
