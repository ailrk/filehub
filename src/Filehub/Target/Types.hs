{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Filehub.Target.Types where


import GHC.Generics (Generic)
import Filehub.Target.File (FileTarget(..))
import Filehub.Target.S3 (S3Target(..))


data Target
  = S3Target S3Target
  | FileTarget FileTarget
  deriving (Generic)


instance Eq Target where
  S3Target a == S3Target b = a.targetId == b.targetId
  FileTarget a == FileTarget b = a.targetId == b.targetId
  _ == _ = False
