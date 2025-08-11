{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Session.Types
  ( SessionId(..),
    Session(..),
    TargetSessionData(..)
  )
  where

import Filehub.Display (Resolution)
import Filehub.UserAgent (DeviceType)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Filehub.Session.Types.SessionId (SessionId(..))
import Filehub.Sort (SortFileBy)
import Filehub.Copy.Types (CopyState)
import Filehub.Selected.Types (Selected)
import Filehub.Layout (Layout)
import Filehub.Theme (Theme)
import Filehub.User (AuthId)


data Session = Session
  { sessionId :: SessionId
  , authId :: Maybe AuthId
  , resolution :: Maybe Resolution
  , deviceType :: DeviceType
  , expireDate :: UTCTime
  , targets :: [TargetSessionData]
  , copyState :: CopyState
  , index :: Int
  , layout :: Layout
  , theme :: Theme
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
