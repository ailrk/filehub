{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cache.Key where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Hashable (Hashable)
import Control.Category ((>>>))


newtype CacheKey = CacheKey ByteString deriving (Show, Eq, Ord, Hashable)


mkCacheKey :: [Builder] -> CacheKey
mkCacheKey =
  intersperse (Builder.charUtf8 ':')
  >>> mconcat
  >>> Builder.toLazyByteString
  >>> LazyByteString.toStrict
  >>> CacheKey
