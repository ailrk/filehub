module Filehub.Session.Handle where

import Target.Types (HasTargetId)
import Filehub.Session.Types.SessionId (SessionId)
import Filehub.Monad (Filehub)
import Filehub.Session.Types (SessionGet)


withTarget :: HasTargetId t => SessionId -> t -> Filehub a -> Filehub a
get :: SessionId -> (SessionGet Filehub -> Filehub a) -> Filehub a
