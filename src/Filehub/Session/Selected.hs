{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Session.Selected
  ( getSelected
  , setSelected
  , anySelected
  , clearSelected
  , clearSelectedAllTargets
  , allSelecteds
  )
  where

import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Reader.Dynamic (Reader, asks)
import Filehub.Session qualified as Session
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Types (Env(..), SessionId, Session(..), Selected(..), TargetSessionData (..))
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Target.Types (AnyTarget)
import Filehub.Monad (Filehub)
import Effectful.Concurrent.STM (readTVarIO)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)


getSelected :: SessionId -> Filehub Selected
getSelected sessionId = (^. #sessionData . #selected) <$> Session.currentTarget sessionId


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
  targets <- asks @Env (.targets) >>= readTVarIO
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


clearSelectedAllTargets :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clearSelectedAllTargets sessionId = do
  let update sessionData = sessionData & #selected .~ NoSelection
  Session.Pool.update sessionId \s -> s &  #targets . mapped %~ update
