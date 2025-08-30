{-# LANGUAGE DeriveGeneric #-}

module Filehub.Target.Types.TargetView where
import Filehub.Target.Types (Target)
import Filehub.Session.Types (TargetSessionData)
import GHC.Generics (Generic)


data TargetView = TargetView
  { target      :: Target
  , sessionData :: TargetSessionData
  , index       :: Int
  }
  deriving (Generic)
