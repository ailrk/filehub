{-# LANGUAGE DeriveGeneric #-}

module Filehub.Target.S3 where

import Filehub.Target.Types (TargetId)
import Filehub.Target.Class (TargetBackend(..))
import Data.Text (Text)
import Amazonka.Env qualified as Amazonka
import GHC.Generics (Generic)


data S3Target = S3Target
  { targetId :: TargetId
  , bucket :: Text
  , env :: Amazonka.Env
  }
  deriving (Generic)



instance TargetBackend S3Target where
  getTargetId = (.targetId)
