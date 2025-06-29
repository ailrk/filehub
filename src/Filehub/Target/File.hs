{-# LANGUAGE DeriveGeneric #-}

module Filehub.Target.File where
import Filehub.Target.Types.TargetId (TargetId)
import Data.Text (Text)
import GHC.Generics (Generic)


data FileTarget = FileTarget_
  { targetId :: TargetId
  , targetName :: Maybe Text
  , root :: FilePath
  }
  deriving (Show, Eq, Generic)
