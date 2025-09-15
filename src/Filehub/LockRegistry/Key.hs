{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.LockRegistry.Key where

import Data.Hashable (Hashable (..))


-- | The key should be derived from the resource identifier it protects. e.g from a filename.
-- Needs to be non crpytographic hash with low collision
newtype LockKey = LockKey Int deriving (Show, Eq, Ord, Hashable)


mkLockKey :: Hashable a => a -> LockKey
mkLockKey a = LockKey (hash a)
