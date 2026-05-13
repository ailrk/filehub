{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Filehub.Session.Selected
  ( elem
  , toList
  , fromList
  , clearSelected
  , clearSelectedAllTargets
  , getAllSelected
  , AllSelected(..)
  )
  where

import Data.ClientPath (ClientPath)
import Data.List (union)
import Data.Map.Strict qualified as M
import Filehub.Monad (Filehub)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Types (Selected(..), SessionGet(..), SessionSet(..), TargetView (..), TargetSessionData (..))
import Filehub.Session.Types (Session(..))
import Filehub.Types (SessionId)
import Prelude hiding (elem)
import Prelude qualified
import Target.Types (AnyTarget)
import {-# SOURCE #-} Filehub.Session.Handle qualified as Session
import Data.Monoid (Sum(..))


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
getAllSelected :: SessionId -> Filehub AllSelected
getAllSelected sessionId = do
  targetViews <- Session.get sessionId (.targetViews)
  let
      content = [ (target, selected)
                | tv@(TargetView target (TargetSessionData { selected })) <- targetViews
                , hasSelection tv
                ]

   in
      pure AllSelected
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


clearSelected :: SessionId -> Filehub ()
clearSelected sessionId = Session.set sessionId (.selected) NoSelection


clearSelectedAllTargets :: SessionId -> Filehub ()
clearSelectedAllTargets sessionId = do
  Session.Pool.update sessionId \s ->
    let
        targets = s.targets
        newTargets = M.map (\t -> t { selected = NoSelection } :: TargetSessionData) targets
     in
        s { targets = newTargets }
