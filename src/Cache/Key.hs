{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Cache.Key (SomeCacheKey(..), CacheKey(..), mkCacheKey) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Hashable (Hashable (..))
import Control.Category ((>>>))
import GHC.Generics (Generic)
import Data.Function (on)


data SomeCacheKey = forall a . SomeCacheKey (CacheKey a)
instance Show     SomeCacheKey where show = show . toRep
instance Eq       SomeCacheKey where (==) = (==) `on` toRep
instance Ord      SomeCacheKey where compare = compare `on` toRep
instance Hashable SomeCacheKey where hashWithSalt salt = hashWithSalt salt . toRep


newtype SomeCacheKeyRep = SomeCacheKeyRep ByteString
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable)

-- Conversion function
toRep :: SomeCacheKey -> SomeCacheKeyRep
toRep (SomeCacheKey (CacheKey bs)) = SomeCacheKeyRep bs


newtype CacheKey a = CacheKey ByteString
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable)


mkCacheKey :: [Builder] -> CacheKey a
mkCacheKey =
  intersperse (Builder.charUtf8 ':')
  >>> mconcat
  >>> Builder.toLazyByteString
  >>> LazyByteString.toStrict
  >>> CacheKey
