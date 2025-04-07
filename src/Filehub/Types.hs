{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Filehub.Types
  ( Session(..)
  , TargetSessionData(..)
  , SessionId(..)
  , SessionPool(..)
  , Env(..)
  , TargetId(..)
  , Target(..)
  , S3Target(..)
  , FileTarget(..)
  )
  where


import Effectful.Concurrent.STM (TVar)
import Data.UUID (UUID)
import Data.Time (UTCTime, NominalDiffTime)
import Data.HashTable.IO (BasicHashTable)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Control.Concurrent.Timer qualified as Timer
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Filehub.Domain.Types (Theme, SortFileBy)
import Network.URI.Encode qualified as URI.Encode
import Amazonka qualified


newtype SessionId = SessionId UUID
  deriving (Show, Eq, Ord, Hashable)


data Session = Session
  { sessionId :: SessionId
  , expireDate :: UTCTime
  , targets :: [TargetSessionData]
  , index :: Int
  }
  deriving (Show, Eq, Generic)


data TargetSessionData = TargetSessionData
  { currentDir :: FilePath
  , sortedFileBy :: SortFileBy
  }
  deriving (Show, Eq, Generic)


data SessionPool = SessionPool
  { pool :: BasicHashTable SessionId Session
  , gc :: Timer.TimerIO
  -- ^ garbage collector, periodically clean up expired sessions.
  }


newtype TargetId = TargetId UUID deriving (Show, Eq, Ord, Hashable)


instance ToHttpApiData TargetId where
  toUrlPiece (TargetId p) = toUrlPiece p


instance FromHttpApiData TargetId where
  parseUrlPiece p = TargetId <$> parseUrlPiece (URI.Encode.decodeText p)



data Target
  = S3Target S3Target
  | FileTarget FileTarget
  deriving (Generic)


data S3Target = S3Target_
  { targetId :: TargetId
  , targetName :: Maybe Text
  , env :: Amazonka.Env
  , uri :: String
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
  , currentRoot :: TVar FilePath
  }
