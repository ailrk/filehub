module Filehub.Session.Selected (anySelected) where

import Filehub.Types (SessionId)
import Filehub.Monad (Filehub)



anySelected :: SessionId -> Filehub Bool
