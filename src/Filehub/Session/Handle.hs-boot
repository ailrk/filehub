module Filehub.Session.Handle where

import Filehub.Session.Types (SessionId, SessionGet, SessionSet)
import Filehub.Monad (Filehub)


get :: SessionId -> (SessionGet Filehub -> Filehub a) -> Filehub a
set :: SessionId -> (SessionSet Filehub -> val -> Filehub ()) -> val -> Filehub ()
newSessionGet :: SessionId -> SessionGet Filehub
newSessionSet :: SessionId -> SessionSet Filehub
