{-# LANGUAGE DeriveGeneric #-}
module Filehub.Session.Types
  ( SessionId(..)
  , Session(..)
  , Selected(..)
  , Storage(..)
  , TargetSessionData(..)
  , Pool(..)
  , TargetView(..)
  , SessionGet(..)
  , SessionSet(..)
  , ControlPanelState(..)
  , CopyState(..)
  , Layout(..)
  )
  where

import Control.Concurrent.Timer qualified as Timer
import Control.Handle.Storage (Storage(..))
import Data.ClientPath (AbsPath, Root, ClientPath)
import Data.File (FileInfo)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Time (UTCTime)
import Filehub.Auth.Types (AuthId)
import Filehub.Auth.Types.OIDC (SomeOIDCFlow)
import Filehub.Display (Resolution, Display)
import Filehub.Locale (Locale)
import Filehub.Notification.Types (Notification(..))
import Filehub.Session.Types.SessionId (SessionId(..))
import Filehub.SharedLink (SharedLinkPermitSet)
import Filehub.Sort (SortFileBy)
import Filehub.Theme (Theme)
import Filehub.UserAgent (DeviceType)
import GHC.Generics (Generic)
import Servant (ToHttpApiData (..), FromHttpApiData (..))
import Target.Types (AnyTarget, TargetId)
import Text.Debug (Debug(..))
import UnliftIO (TBQueue, TVar, STM)
import Web.FormUrlEncoded (FromForm, parseAll)
import Web.Internal.FormUrlEncoded (FromForm(..))
import Worker.Task (TaskId)


data Session = Session
  { sessionId         :: SessionId
  , authId            :: Maybe AuthId
  , sharedLinkPermit  :: Maybe SharedLinkPermitSet
  , resolution        :: Maybe Resolution
  , deviceType        :: DeviceType
  , expireDate        :: UTCTime
  , targets           :: Map TargetId TargetSessionData
  , copyState         :: CopyState
  , currentTargetId   :: TVar TargetId
  , sidebarCollapsed  :: Bool
  , layout            :: Layout
  , theme             :: Theme
  , locale            :: Locale
  , oidcFlow          :: Maybe SomeOIDCFlow
  , notifications     :: TBQueue Notification
  , pendingTasks      :: TVar (Set TaskId)
  }
  deriving (Generic)


instance Eq Session where
  a == b = a.sessionId == b.sessionId


data SessionGet m = SessionGet
  { currentDir        :: m AbsPath
  , root              :: m Root
  , display           :: m Display
  , sortedFileBy      :: m SortFileBy
  , selected          :: m Selected
  , authId            :: m (Maybe AuthId)
  , resolution        :: m (Maybe Resolution)
  , deviceType        :: m DeviceType
  , sidebarCollapsed  :: m Bool
  , layout            :: m Layout
  , theme             :: m Theme
  , locale            :: m Locale
  , copyState         :: m CopyState
  , targetViews       :: m (STM [TargetView])
  , controlPanelState :: m (ControlPanelState)
  , sharedLinkPermit  :: m (Maybe SharedLinkPermitSet)
  , oidcFlow          :: m (Maybe SomeOIDCFlow)
  , notifications     :: m (TBQueue Notification)
  , pendingTasks      :: m (TVar (Set TaskId))
  , storage           :: m (Storage m)
  , currentTarget     :: m (STM TargetView)
  }


data SessionSet m = SessionSet
  { currentDir        :: AbsPath -> m ()
  , sortedFileBy      :: SortFileBy -> m ()
  , selected          :: Selected -> m  ()
  , authId            :: Maybe AuthId -> m ()
  , sidebarCollapsed  :: Bool -> m ()
  , layout            :: Layout -> m ()
  , resolution        :: Maybe Resolution -> m ()
  , deviceType        :: DeviceType -> m ()
  , theme             :: Theme -> m ()
  , locale            :: Locale -> m ()
  , copyState         :: CopyState -> m ()
  , sharedLinkPermit  :: Maybe SharedLinkPermitSet -> m ()
  , currentTarget     :: TargetId -> m (STM ())
  , oidcFlow          :: Maybe SomeOIDCFlow -> m ()
  , notifications     :: TBQueue Notification -> m ()
  , pendingTasks      :: TVar (Set TaskId) -> m ()
  }


------------------------------
-- Session Pool


data Pool = Pool
  { pool :: TVar (Map SessionId Session)
  , gc   :: Timer.TimerIO
  -- ^ garbage collector, periodically clean up expired sessions.
  }


------------------------------
-- UI State


data Selected
  = Selected ClientPath [ClientPath] -- non empty list
  | NoSelection
  deriving (Show, Eq, Generic, Debug)


instance FromForm Selected where
  fromForm f = do
    selected <- parseAll "selected" f
    case selected of
      []   -> pure NoSelection
      x:xs -> pure $ Selected x xs


-- | State machine reprents the copy and paste process.
data CopyState
 -- | Ready to paste
  = CopySelected [(AnyTarget, [FileInfo])]
  -- | Start pasting files to target path
  | Paste [(AnyTarget, [FileInfo])]
  -- | No copy paste action being performed at the moment.
  | NoCopyPaste
  deriving (Generic, Debug)


-- | The table layout of a session
data Layout
  = ThumbnailLayout
  | ListLayout
  deriving (Show, Eq)


instance ToHttpApiData Layout where
  toUrlPiece ThumbnailLayout = "ThumbnailLayout"
  toUrlPiece ListLayout      = "ListLayout"


instance FromHttpApiData Layout where
  parseUrlPiece "ThumbnailLayout" = pure ThumbnailLayout
  parseUrlPiece "ListLayout"      = pure ListLayout
  parseUrlPiece _                 = Left "Unknown layout"


-- | The state of the control panel.
data ControlPanelState
  = ControlPanelDefault
  | ControlPanelSelecting
  | ControlPanelCopied
  deriving (Show, Eq)


------------------------------
-- Target View


data TargetView = TargetView
  { target      :: AnyTarget
  , sessionData :: TargetSessionData
  }
  deriving (Generic, Debug)


data TargetSessionData = TargetSessionData
  { currentDir   :: AbsPath
  , sortedFileBy :: SortFileBy
  , selected     :: Selected
  }
  deriving (Generic, Debug)
