{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Session.Types where

import Data.UUID (UUID)
import Data.Hashable (Hashable)
import Filehub.Display (Resolution)
import Filehub.UserAgent (DeviceType)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Filehub.Sort (SortFileBy)
import Filehub.Copy.Types (CopyState)
import Filehub.Selected.Types (Selected)


newtype SessionId = SessionId UUID
  deriving (Show, Eq, Ord, Hashable)


data Session = Session
  { sessionId :: SessionId
  , resolution :: Maybe Resolution
  , deviceType :: DeviceType
  , expireDate :: UTCTime
  , targets :: [TargetSessionData]
  , copyState :: CopyState
  , index :: Int
  }
  deriving (Generic)


instance Eq Session where
  a == b = a.sessionId == b.sessionId


data TargetSessionData = TargetSessionData
  { currentDir :: FilePath
  , sortedFileBy :: SortFileBy
  , selected :: Selected
  }
  deriving (Generic)
