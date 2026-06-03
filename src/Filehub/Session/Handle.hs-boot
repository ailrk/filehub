module Filehub.Session.Handle where

import Target.Types (HasTargetId)
import Filehub.Session.Types.SessionId (SessionId)
import Filehub.Monad (Filehub)


withTarget :: HasTargetId t => SessionId -> t -> Filehub a -> Filehub a
