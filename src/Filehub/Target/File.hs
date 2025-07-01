module Filehub.Target.File where

import Filehub.Target.Types.TargetId (TargetId)
import Filehub.Target.Class (IsTarget (..))
import Data.Text (Text)


data FileSys


instance IsTarget FileSys where
  data Backend FileSys =
    FileBackend
      { targetId :: TargetId
      , targetName :: Maybe Text
      , root :: FilePath
      }
  getTargetIdFromBackend f = f.targetId
