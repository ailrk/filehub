{-# LANGUAGE DeriveGeneric #-}

module Filehub.Target.File where

import Filehub.Target.Types (TargetId)
import Filehub.Target.Class (TargetBackend(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Filehub.Storage.S3 (storage)


data FileTarget = FileTarget
  { targetId :: TargetId
  , targetName :: Maybe Text
  , root :: FilePath
  }
  deriving (Generic)



instance TargetBackend FileTarget where
  getTargetId = (.targetId)
  getStorage _ = storage
