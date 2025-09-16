-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the pure part of file selection logic.
module Filehub.Selected
  ( anySelected
  , allSelecteds
  , countSelected
  , toList
  , fromList
  , elem
  , AsSet(..)
  ) where

import Control.Monad (join)
import Data.List (union)
import Filehub.Types (ClientPath, Session(..), Selected(..))
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude qualified
import Target.Types (Target)


anySelected :: Session -> Bool
anySelected session =
  session
  ^. #targets
  & fmap (^. #selected)
  & any (\case { Selected _ _ -> True; NoSelection -> False })


allSelecteds :: [Selected] -> [Target] -> [(Target, Selected)]
allSelecteds selecteds targets = targets `zip` selecteds


countSelected :: [Selected] -> [Target] -> Int
countSelected selecteds targets =
  length . join . fmap (toList . snd) $ allSelecteds selecteds targets


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
