module Filehub.Session.Copy (getCopyState) where

import Filehub.Error (FilehubError (..))
import Filehub.Types (CopyState(..), SessionId, Env)


getCopyState :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es CopyState
