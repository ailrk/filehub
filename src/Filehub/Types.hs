{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Filehub.Types
  ( Session(..)
  , TargetSessionData(..)
  , Selected(..)
  , CopyState(..)
  , ControlPanelState(..)
  , SessionId(..)
  , SessionPool(..)
  , Env(..)
  , Resolution(..)
  , Display(..)
  , TargetId(..)
  , Target(..)
  , S3Target(..)
  , FileTarget(..)
  , FileContent(..)
  , File(..)
  , ClientPath(..)
  , RawClientPath(..)
  , SortFileBy(..)
  , SearchWord(..)
  , NewFile(..)
  , NewFolder(..)
  , UpdatedFile(..)
  , Theme(..)
  , FilehubEvent(..)
  , Resource(..)
  , Manifest
  )
  where


import Control.Concurrent.Timer qualified as Timer
import Data.HashTable.IO (BasicHashTable)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Lazy.Encoding qualified as LText
import Data.Time (UTCTime, NominalDiffTime)
import Data.UUID (UUID)
import Data.Aeson (ToJSON (..), (.=), Value)
import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Platform ()
import Log (Logger, LogLevel)
import Servant
    ( ToHttpApiData(..),
      Accept (..),
      MimeRender )
import Web.FormUrlEncoded (FromForm (..), parseUnique, parseAll)
import Filehub.UserAgent (DeviceType)
import Servant.API (MimeRender(..))
import Filehub.Target.Types (Target (..))
import Filehub.Target.S3 (S3Target(..))
import Filehub.Target.File (FileTarget(..))
import Filehub.Target.Types.TargetId (TargetId(..))
import Filehub.ClientPath (ClientPath(..), RawClientPath(..))
import Filehub.File (File(..), FileContent(..))
import Filehub.Theme (Theme(..))
import Filehub.Display (Display(..), Resolution(..))
import Filehub.Sort (SortFileBy(..))


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


data Selected
  = Selected ClientPath [ClientPath] -- non empty list
  | NoSelection
  deriving (Show, Eq)


instance FromForm Selected where
  fromForm f = do
    selected <- parseAll "selected" f
    case selected of
      [] -> pure NoSelection
      x:xs->  pure $ Selected x xs


data SessionPool = SessionPool
  { pool :: BasicHashTable SessionId Session
  , gc :: Timer.TimerIO
  -- ^ garbage collector, periodically clean up expired sessions.
  }


data CopyState
 -- | Ready to paste
  = CopySelected [(Target, [File])]
  -- | Start pasting files to target path
  | Paste [(Target, [File])]
  -- | No copy paste action being performed at the moment.
  | NoCopyPaste


data ControlPanelState
  = ControlPanelDefault
  | ControlPanelSelecting
  | ControlPanelCopied


data Env = Env
  { port :: !Int
  , theme :: Theme
  , dataDir :: !FilePath
  , sessionPool :: SessionPool
  , sessionDuration :: NominalDiffTime
  , targets :: [Target]
  , readOnly :: Bool
  , logger :: Logger
  , logLevel :: LogLevel
  }


newtype SearchWord = SearchWord Text deriving (Show, Eq, Generic)
instance FromForm SearchWord where fromForm f = SearchWord <$> parseUnique "search" f


newtype NewFile = NewFile Text deriving (Show, Eq, Generic)
instance FromForm NewFile where fromForm f = NewFile <$> parseUnique "new-file" f


newtype NewFolder = NewFolder Text deriving (Show, Eq, Generic)
instance FromForm NewFolder where fromForm f = NewFolder <$> parseUnique "new-folder" f


data UpdatedFile = UpdatedFile
  { clientPath :: ClientPath
  , content :: Text
  }
  deriving (Show, Eq, Generic)
instance FromForm UpdatedFile where
  fromForm f = do
    path <- parseUnique "path" f
    content <- parseUnique "content" f
    pure (UpdatedFile path content)


data Resource = Resource
  { url :: RawClientPath
  , mimetype :: Text
  }
  deriving (Show, Eq)


instance ToJSON Resource where
  toJSON (Resource { url = RawClientPath path, mimetype }) = toJSON [ toJSON path , toJSON mimetype ]


data FilehubEvent
  = ViewerInited [Resource] Int -- Update image list and show the viewer
  | TargetChanged
  | TableSorted
  | DirChanged
  | Canceled -- Action canceled
  deriving (Show)


instance ToJSON FilehubEvent where
  toJSON (ViewerInited res index) =
    Aeson.object
      [ "ViewerInited" .= Aeson.object
          [ "resources" .= toJSON res
          , "index" .= toJSON index
          ]
      ]
  toJSON TargetChanged = Aeson.object [ "TargetChanged" .= Aeson.object [] ]
  toJSON TableSorted = Aeson.object [ "TableSorted" .= Aeson.object [] ]
  toJSON DirChanged = Aeson.object [ "DirChanged" .= Aeson.object [] ]
  toJSON Canceled = Aeson.object [ "Canceled" .= Aeson.object [] ]


instance ToHttpApiData FilehubEvent where
  toUrlPiece v = (v & Aeson.encode & LText.decodeUtf8) ^. strict


data Manifest


instance Accept Manifest where
  contentType _ = "application/manifest+json"


instance MimeRender Manifest Value where
  mimeRender _ = Aeson.encode
