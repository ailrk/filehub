module Cache.Dummy where

import Cache.Key (CacheKey)


lookup :: CacheKey -> IO (Maybe a)
lookup _ = pure Nothing


insert :: CacheKey -> a -> IO ()
insert _ _ = pure ()


delete :: CacheKey -> IO ()
delete _ = pure ()


flush :: IO ()
flush = pure ()

