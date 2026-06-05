{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Filehub.Session.Selected
  ( elem
  , toList
  , fromList
  , clearSelectedAllTargets
  , getAllSelected
  , AllSelected(..)
  )
  where

import Data.ClientPath (ClientPath)
import Data.List (union)
import Data.Monoid (Sum(..))
import Filehub.Monad (Filehub)
import Filehub.Session.Types (Selected(..), TargetView (..), TargetSessionData (..))
import Filehub.Session.Types (Session(..))
import Filehub.Types (SessionId)
import Prelude hiding (elem)
import Prelude qualified
import Target.Types (AnyTarget)
import Filehub.Session.Pool (withSession)
import UnliftIO (STM, readTVar, modifyTVar)
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)


toList :: Selected -> [ClientPath]
toList NoSelection = mempty
toList (Selected x xs) = x:xs


fromList :: [ClientPath] -> Selected
fromList (x:xs) = Selected x xs
fromList []     = NoSelection


elem :: ClientPath -> Selected -> Bool
elem _ NoSelection        = False
elem path (Selected x xs) = path == x || path `Prelude.elem` xs


instance Semigroup Selected where
  a <> b = fromList (toList a `union` toList b)


instance Monoid Selected where
  mempty = NoSelection


data AllSelected = AllSelected
  { allSelected :: [(AnyTarget, Selected)]
  , count       :: Int
  }


-- | Get all selected files grouped by targets
getAllSelected :: [TargetView] -> STM AllSelected
getAllSelected targetViews = do
  mContent <- for targetViews \(TargetView t (TargetSessionData { selected })) -> do
    selected' <- readTVar selected
    case selected' of
      NoSelection -> pure Nothing
      _           -> pure (Just (t, selected'))

  let content = catMaybes mContent
  pure
    AllSelected
      { allSelected = content
      , count       = totalSelected content
      }


totalSelected :: [(AnyTarget, Selected)] -> Int
totalSelected as = do
  let
      ss           = fmap (toList . snd) as
      (Sum result) = foldMap (Sum . length) ss
   in
      result


clearSelectedAllTargets :: SessionId -> Filehub ()
clearSelectedAllTargets sessionId = withSession sessionId \s ->  do
  for_ s.targets \t -> do
    modifyTVar t.selected (const NoSelection)
