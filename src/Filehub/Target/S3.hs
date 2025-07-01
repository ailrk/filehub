module Filehub.Target.S3 where

import Filehub.Target.Types.TargetId (TargetId)
import Data.Text (Text)
import Amazonka.Env qualified as Amazonka
import Filehub.Target.Class (IsTarget (..))


data S3


instance IsTarget S3 where
  data Backend S3 = S3Backend
    { targetId :: TargetId
    , bucket :: Text
    , env :: Amazonka.Env
    }
  getTargetIdFromBackend f = f.targetId
