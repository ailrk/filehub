module Filehub.Storage.File where

import Filehub.Session.Types (SessionId)
import Target.Storage (Storage(..))
import Filehub.Monad (Filehub, IsFilehub)
import Effectful (Eff)


storage :: IsFilehub es => SessionId -> Storage (Eff es)
