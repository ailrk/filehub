module Filehub.Session.Copy (getCopyState) where

import Filehub.Types (CopyState(..), SessionId)
import Filehub.Monad (Filehub)


getCopyState :: SessionId -> Filehub CopyState
