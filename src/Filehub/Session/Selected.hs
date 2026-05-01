{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Session.Selected
  ( elem
  , toList
  , fromList
  , setSelected
  , anySelected
  , clearSelected
  , clearSelectedAllTargets
  , allSelecteds
  )
  where

import Control.Monad.Reader (asks)
import Data.ClientPath (ClientPath)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Filehub.Monad (Filehub)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Types (Env(..), SessionId, Session(..), TargetSessionData (..))
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Target.Types (AnyTarget)
import UnliftIO.STM (readTVarIO)
import Filehub.Session.Types (Selected(..))
import Prelude qualified
import Data.List (union)


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


setSelected :: SessionId -> Selected -> Filehub ()
setSelected sessionId selected = Session.Pool.update sessionId \s -> s & #targets . ix s.currentTargetId . #selected .~ selected


anySelected :: SessionId -> Filehub Bool
anySelected sessionId = go <$> Session.Pool.get sessionId
  where
    go :: Session -> Bool
    go session = session ^. #targets & fmap (^. #selected) & any (\case { Selected _ _ -> True; NoSelection -> False })


-- | Get all selected files grouped by targets
allSelecteds :: SessionId -> Filehub [(AnyTarget, Selected)]
allSelecteds sessionId = do
  session <- Session.Pool.get sessionId
  targets <- asks (.targets) >>= readTVarIO
  session ^. #targets
    & Map.toList
    & filter hasSelection
    & mapM (go targets)
    <&> catMaybes
  where
    hasSelection (_, TargetSessionData { selected })
      | NoSelection <- selected = False
      | otherwise               = True

    go targets (targetId, TargetSessionData { selected }) = do
      case lookup targetId targets of
        Just target -> pure $ Just (target, selected)
        Nothing -> pure Nothing


clearSelected :: SessionId -> Filehub ()
clearSelected sessionId = setSelected sessionId NoSelection


clearSelectedAllTargets :: SessionId -> Filehub ()
clearSelectedAllTargets sessionId = do
  let update sessionData = sessionData & #selected .~ NoSelection
  Session.Pool.update sessionId \s -> s &  #targets . mapped %~ update
