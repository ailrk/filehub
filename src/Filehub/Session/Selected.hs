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

import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Log (Log)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader, asks)
import Filehub.Types (Env(..), SessionId, Session(..), Selected(..), Target)
import Filehub.Error (FilehubError)
import Filehub.Session qualified as Session
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Selected qualified as Selected
import Prelude hiding (elem)


getSelected :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es Selected
getSelected sessionId = (^. #sessionData . #selected) <$> Session.currentTarget sessionId


setSelected :: (Reader Env :> es, IOE :> es) => SessionId -> Selected -> Eff es ()
setSelected sessionId selected = Session.Pool.update sessionId \s -> s & #targets . ix s.index . #selected .~ selected


anySelected :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es Bool
anySelected sessionId = Selected.anySelected  <$> Session.Pool.get sessionId


-- | Get all selected files grouped by targets
allSelecteds :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es [(Target, Selected)]
allSelecteds sessionId = do
  session <- Session.Pool.get sessionId
  let selecteds = session ^. #targets & fmap (^. #selected)
  targets <- asks @Env (.targets)
  pure (Selected.allSelecteds selecteds targets)


countSelected :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es Int
countSelected sessionId = do
  session <- Session.Pool.get sessionId
  let selecteds = session ^. #targets & fmap (^. #selected)
  targets <- asks @Env (.targets)
  pure $ Selected.countSelected selecteds targets


clearSelected :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clearSelected sessionId = setSelected sessionId NoSelection


clearSelectedAllTargets :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clearSelectedAllTargets sessionId = do
  let update sessionData = sessionData & #selected .~ NoSelection
  Session.Pool.update sessionId \s -> s &  #targets . mapped %~ update
