{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Session.Types
  ( SessionId(..)
  , Session(..)
  , TargetSessionData(..)
  , Pool(..)
  , TargetView(..)
  )
  where

import Control.Concurrent.Timer qualified as Timer
import Data.HashTable.IO (BasicHashTable)
import Data.Set (Set)
import Data.Time (UTCTime)
import Filehub.Auth.Types (AuthId)
import Filehub.Display (Resolution)
import Filehub.Locale (Locale)
import Filehub.Notification.Types (Notification(..))
import Filehub.Session.Types.SessionId (SessionId(..))
import Filehub.Sort (SortFileBy)
import Filehub.Theme (Theme)
import Filehub.UserAgent (DeviceType)
import GHC.Generics (Generic)
import Target.Types (Target)
import UnliftIO (TBQueue, TVar)
import Worker.Task (TaskId)
import {-# SOURCE #-} Filehub.Auth.OIDC (SomeOIDCFlow)
import {-# SOURCE #-} Filehub.Types (Layout(..), CopyState(..), Selected)


data Session = Session
  { sessionId     :: SessionId
  , authId        :: Maybe AuthId
  , resolution    :: Maybe Resolution
  , deviceType    :: DeviceType
  , expireDate    :: UTCTime
  , targets       :: [TargetSessionData]
  , copyState     :: CopyState
  , index         :: Int
  , layout        :: Layout
  , theme         :: Theme
  , locale        :: Locale
  , oidcFlow      :: Maybe SomeOIDCFlow
  , notifications :: TBQueue Notification
  , pendingTasks  :: TVar (Set TaskId)
  }
  deriving (Generic)


instance Eq Session where
  a == b = a.sessionId == b.sessionId


data TargetView = TargetView
  { target      :: Target
  , sessionData :: TargetSessionData
  , index       :: Int
  }
  deriving (Generic)


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
