module Filehub.Session.Selected (anySelected) where

import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Log (Log)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Error (FilehubError)
import Filehub.Types (Env(..))
import Filehub.Types (SessionId)



anySelected :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es Bool
