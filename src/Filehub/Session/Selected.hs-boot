module Filehub.Session.Selected (anySelected) where

import Lens.Micro.Platform ()
import Filehub.Types (SessionId)
import Prelude hiding (elem)
import Filehub.Monad (Filehub)


anySelected :: SessionId -> Filehub Bool
