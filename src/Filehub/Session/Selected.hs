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
import Data.Map.Strict qualified as M
import Data.Monoid (Sum(..))
import Filehub.Monad (Filehub)
import Filehub.Session.Types (Selected(..), TargetView (..), TargetSessionData (..))
import Filehub.Session.Types (Session(..))
import Filehub.Types (SessionId)
import Prelude hiding (elem)
import Prelude qualified
import Target.Types (AnyTarget)
import Filehub.Session.Pool (modifySession)


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
getAllSelected :: [TargetView] -> AllSelected
getAllSelected targetViews =
  let
      content = [ (target, selected)
                | tv@(TargetView target (TargetSessionData { selected })) <- targetViews
                , hasSelection tv
                ]

   in
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


hasSelection :: TargetView -> Bool
hasSelection TargetView { sessionData = TargetSessionData { selected } }
  | NoSelection <- selected = False
  | otherwise               = True


clearSelectedAllTargets :: SessionId -> Filehub ()
clearSelectedAllTargets sessionId = modifySession sessionId \s ->  do
    let
        targets    = s.targets
        newTargets = M.map (\t -> t { selected = NoSelection } :: TargetSessionData) targets
     in
        pure s { targets = newTargets }
