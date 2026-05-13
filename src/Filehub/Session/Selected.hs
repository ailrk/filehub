{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Session.Selected
  ( elem
  , toList
  , fromList
  , anySelected
  , clearSelected
  , clearSelectedAllTargets
  , allSelecteds
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


anySelected :: SessionId -> Filehub Bool
anySelected sessionId = do
  targetViews <- Session.get sessionId (.targetViews)

  pure $ or (fmap hasSelection targetViews)


-- | Get all selected files grouped by targets
allSelecteds :: SessionId -> Filehub [(AnyTarget, Selected)]
allSelecteds sessionId = do
  targetViews <- Session.get sessionId (.targetViews)

  pure
    [ (target, selected)
    | tv@(TargetView target (TargetSessionData { selected })) <- targetViews
    , hasSelection tv
    ]


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
