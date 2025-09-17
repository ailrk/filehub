module Cache.Dummy where

import Cache.Key (CacheKey, SomeCacheKey)
import Data.Time (NominalDiffTime)


lookup :: CacheKey a -> IO (Maybe a)
lookup _ = pure Nothing


insert :: CacheKey a -> [SomeCacheKey] -> Maybe NominalDiffTime -> a -> IO ()
insert _ _ _ _ = pure ()


delete :: SomeCacheKey -> IO ()
delete _ = pure ()


flush :: IO ()
flush = pure ()
