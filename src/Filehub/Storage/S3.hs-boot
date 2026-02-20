module Filehub.Storage.S3 where

import Filehub.Session.Types (SessionId)
import Target.Storage (Storage(..))
import Filehub.Monad (Filehub)


storage :: SessionId -> Storage Filehub
