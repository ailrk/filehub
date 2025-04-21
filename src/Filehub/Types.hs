{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Filehub.Types
  ( Session(..)
  , TargetSessionData(..)
  , Selected(..)
  , CopyState(..)
  , SessionId(..)
  , SessionPool(..)
  , Env(..)
  , TargetId(..)
  , Target(..)
  , S3Target(..)
  , FileTarget(..)
  )
  where


import Data.UUID (UUID)
import Data.Time (UTCTime, NominalDiffTime)
import Data.HashTable.IO (BasicHashTable)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Control.Concurrent.Timer qualified as Timer
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Filehub.Domain.Types (Theme, SortFileBy, File)
import Network.URI.Encode qualified as URI.Encode
import Amazonka qualified
import Filehub.Domain (ClientPath)


newtype SessionId = SessionId UUID
  deriving (Show, Eq, Ord, Hashable)


data Session = Session
  { sessionId :: SessionId
  , expireDate :: UTCTime
  , targets :: [TargetSessionData]
  , index :: Int
  }
  deriving (Generic)


instance Eq Session where
  a == b = a.sessionId == b.sessionId


data TargetSessionData = TargetSessionData
  { currentDir :: FilePath
  , sortedFileBy :: SortFileBy
  , copyState :: CopyState
  , selected :: Selected
  }
  deriving (Generic)


data Selected
  = Selected ClientPath [ClientPath]
  | NoSelection
  deriving (Show, Eq)


data SessionPool = SessionPool
  { pool :: BasicHashTable SessionId Session
  , gc :: Timer.TimerIO
  -- ^ garbage collector, periodically clean up expired sessions.
  }


type From = Target
type To = Target


data CopyState
  -- | Select files to copy
  = CopySelect [(From, [File])] [File]
  -- | Ready to paste
  | CopySelected [(From, [File])]
  -- | Start pasting files to target path
  | Paste [(From, [File])] To FilePath
  -- | The previous copy paste has been completed
  | CopyFinished
  -- | No copy paste action being performed at the moment.
  | NoCopyPaste


newtype TargetId = TargetId UUID deriving (Show, Eq, Ord, Hashable)


instance ToHttpApiData TargetId where
  toUrlPiece (TargetId p) = toUrlPiece p


instance FromHttpApiData TargetId where
  parseUrlPiece p = TargetId <$> parseUrlPiece (URI.Encode.decodeText p)



data Target
  = S3Target S3Target
  | FileTarget FileTarget
  deriving (Generic)


instance Eq Target where
  S3Target a == S3Target b = a.targetId == b.targetId
  FileTarget a == FileTarget b = a.targetId == b.targetId
  _ == _ = False


data S3Target = S3Target_
  { targetId :: TargetId
  , bucket :: Text
  , env :: Amazonka.Env
  }
  deriving (Generic)


data FileTarget = FileTarget_
  { targetId :: TargetId
  , targetName :: Maybe Text
  , root :: FilePath
  }
  deriving (Show, Eq, Generic)


data Env = Env
  { port :: !Int
  , theme :: Theme
  , dataDir :: !FilePath
  , sessionPool :: SessionPool
  , sessionDuration :: NominalDiffTime
  , targets :: [Target]
  }
