-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the pure part of file selection logic.
module Filehub.Selected
  ( toList
  , fromList
  , elem
  , AsSet(..)
  ) where

import Data.List (union)
import Filehub.Types (ClientPath, Selected(..))
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude qualified


toList :: Selected -> [ClientPath]
toList NoSelection = mempty
toList (Selected x xs) = x:xs


fromList :: [ClientPath] -> Selected
fromList (x:xs) = Selected x xs
fromList []     = NoSelection


elem :: ClientPath -> Selected -> Bool
elem _ NoSelection        = False
elem path (Selected x xs) = path == x || path `Prelude.elem` xs


newtype AsSet = AsSet Selected


instance Semigroup AsSet where
  (AsSet a) <> (AsSet b) = AsSet (fromList (toList a `union` toList b))


instance Monoid AsSet where
  mempty = AsSet NoSelection
