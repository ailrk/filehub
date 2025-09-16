{-# LANGUAGE DeriveGeneric #-}

module Target.Types.TargetView where
import Target.Types (Target)
import Filehub.Session.Types (TargetSessionData)
import GHC.Generics (Generic)


data TargetView = TargetView
  { target      :: Target
  , sessionData :: TargetSessionData
  , index       :: Int
  }
  deriving (Generic)
