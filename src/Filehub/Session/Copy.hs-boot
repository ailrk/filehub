module Filehub.Session.Copy (getCopyState) where

import Lens.Micro.Platform ()
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader)
import Effectful.Log (Log)
import Filehub.Types (Env, CopyState(..), SessionId)
import Filehub.Error (FilehubError (..))


getCopyState :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es CopyState
