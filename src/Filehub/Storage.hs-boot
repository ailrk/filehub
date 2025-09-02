module Filehub.Storage where

import Filehub.Storage.Context qualified as Storage
import Filehub.Storage.Types (Storage)
import Filehub.Types (SessionId)
import Effectful ( Eff, Eff )


getStorage :: Storage.Context es => SessionId -> Eff es (Storage (Eff es))
