module Cache.Dummy where

import Cache.Key (CacheKey)
import Data.Time (NominalDiffTime)


lookup :: CacheKey -> IO (Maybe a)
lookup _ = pure Nothing


insert :: CacheKey -> Maybe NominalDiffTime -> a -> IO ()
insert _ _ _ = pure ()


delete :: CacheKey -> IO ()
delete _ = pure ()


flush :: IO ()
flush = pure ()
