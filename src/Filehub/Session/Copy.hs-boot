module Filehub.Session.Copy (getCopyState) where

import Lens.Micro.Platform ()
import Filehub.Types (CopyState(..), SessionId)
import Filehub.Monad (Filehub)


getCopyState :: SessionId -> Filehub CopyState
