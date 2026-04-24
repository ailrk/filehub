module Filehub.Storage.File where

import Filehub.Session.Types (SessionId)
import Target.Storage (Storage(..))
import Filehub.Monad (Filehub, IsFilehub)


storage :: IsFilehub es => SessionId -> Storage (Eff es)
