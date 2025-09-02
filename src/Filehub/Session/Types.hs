{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Session.Types
  ( SessionId(..)
  , Session(..)
  , TargetSessionData(..)
  , Pool(..)
  )
  where

import Filehub.Display (Resolution)
import Filehub.UserAgent (DeviceType)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Filehub.Session.Types.SessionId (SessionId(..))
import Filehub.Sort (SortFileBy)
import {-# SOURCE #-} Filehub.Types (Layout(..))
import Filehub.Copy.Types (CopyState)
import Filehub.Selected.Types (Selected)
import Filehub.Theme (Theme)
import Filehub.Auth.Types (AuthId)
import Control.Concurrent.Timer qualified as Timer
import Data.HashTable.IO (BasicHashTable)
import Filehub.Locale (Locale)
import Data.Text (Text)


data Session = Session
  { sessionId  :: SessionId
  , authId     :: Maybe AuthId
  , resolution :: Maybe Resolution
  , deviceType :: DeviceType
  , expireDate :: UTCTime
  , targets    :: [TargetSessionData]
  , copyState  :: CopyState
  , index      :: Int
  , layout     :: Layout
  , theme      :: Theme
  , locale     :: Locale
  , oidcState  :: Maybe Text
  }
  deriving (Generic)


instance Eq Session where
  a == b = a.sessionId == b.sessionId


data TargetSessionData = TargetSessionData
  { currentDir   :: FilePath
  , sortedFileBy :: SortFileBy
  , selected     :: Selected
  }
  deriving (Generic)


data Pool = Pool
  { pool :: BasicHashTable SessionId Session
  , gc   :: Timer.TimerIO
  -- ^ garbage collector, periodically clean up expired sessions.
  }
