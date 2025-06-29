{-# LANGUAGE DeriveGeneric #-}

module Filehub.Target.S3 where
import Filehub.Target.Types.TargetId (TargetId)
import Data.Text (Text)
import Amazonka.Env qualified as Amazonka
import GHC.Generics (Generic)


data S3Target = S3Target_
  { targetId :: TargetId
  , bucket :: Text
  , env :: Amazonka.Env
  }
  deriving (Generic)
