module Filehub.Session.Selected (anySelected) where

import Lens.Micro.Platform ()
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Log (Log)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Types (Env(..), SessionId)
import Filehub.Error (FilehubError)
import Prelude hiding (elem)


anySelected :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es Bool
