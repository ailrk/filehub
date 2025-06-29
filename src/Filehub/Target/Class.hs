module Filehub.Target.Class where
import Filehub.Target.Types.TargetId (TargetId)
import Filehub.Storage.Types (Storage)
import Data.Kind (Type)
import Effectful (Eff)
import Filehub.Storage.Context qualified as Storage


class TargetBackend b where
  getTargetId :: b -> TargetId
  getStorage :: Storage.Context es => b -> Storage (Eff es)
