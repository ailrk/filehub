module Filehub.Session.Selected
  ( getSelected
  , setSelected
  , anySelected
  , countSelected
  , clearSelected
  , clearSelectedAllTargets
  , allSelecteds
  )
  where

import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Reader.Dynamic (Reader, asks)
import Filehub.Selected qualified as Selected
import Filehub.Session qualified as Session
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Types (Env(..), SessionId, Session(..), Selected(..))
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Target.Types (Target)
import Filehub.Monad (Filehub)
import Effectful.Concurrent.STM (readTVarIO)
import Data.Map.Strict qualified as Map


getSelected :: SessionId -> Filehub Selected
getSelected sessionId = (^. #sessionData . #selected) <$> Session.currentTarget sessionId


setSelected :: SessionId -> Selected -> Filehub ()
setSelected sessionId selected = Session.Pool.update sessionId \s -> s & #targets . ix s.currentTargetId . #selected .~ selected


anySelected :: SessionId -> Filehub Bool
anySelected sessionId = Selected.anySelected  <$> Session.Pool.get sessionId


-- | Get all selected files grouped by targets
allSelecteds :: SessionId -> Filehub [(Target, Selected)]
allSelecteds sessionId = do
  session <- Session.Pool.get sessionId
  let selecteds = session ^. #targets & Map.elems . fmap (^. #selected)
  targets <- asks @Env (.targets) >>= readTVarIO
  pure (Selected.allSelecteds selecteds (fmap snd targets))


countSelected :: SessionId -> Filehub Int
countSelected sessionId = do
  session <- Session.Pool.get sessionId
  let selecteds = session ^. #targets & Map.elems . fmap (^. #selected)
  targets <- asks @Env (.targets) >>= readTVarIO
  pure $ Selected.countSelected selecteds (fmap snd targets)


clearSelected :: SessionId -> Filehub ()
clearSelected sessionId = setSelected sessionId NoSelection


clearSelectedAllTargets :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clearSelectedAllTargets sessionId = do
  let update sessionData = sessionData & #selected .~ NoSelection
  Session.Pool.update sessionId \s -> s &  #targets . mapped %~ update
