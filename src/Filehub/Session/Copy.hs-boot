module Filehub.Session.Copy (getCopyState) where

import Effectful.Error.Dynamic (Error)
import Effectful.Log (Log)
import Filehub.Error (FilehubError (..))
import Filehub.Types (CopyState(..), SessionId, Env)
import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Dynamic (Reader)


getCopyState :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es CopyState
