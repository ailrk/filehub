{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Types where


import Filehub.Domain.Types (Theme, SortFileBy)
import Data.UUID (UUID)
import Data.Time (UTCTime, NominalDiffTime)
import Data.HashTable.IO (BasicHashTable)
import Data.Hashable (Hashable)
import Control.Concurrent.Timer qualified as Timer


newtype SessionId = SessionId UUID
  deriving (Show, Eq, Ord, Hashable)


data Session = Session
  { sessionId :: SessionId
  , expireDate :: UTCTime
  , currentDir :: FilePath
  , sortedFileBy :: SortFileBy
  }
  deriving (Eq)


data SessionPool = SessionPool
  { pool :: BasicHashTable SessionId Session
  , gc :: Timer.TimerIO
  -- ^ garbage collector, periodically clean up expired sessions.
  }


data Env = Env
  { root :: !FilePath
  , port :: !Int
  , dataDir :: !FilePath
  , theme :: Theme
  , sessionPool :: SessionPool
  , sessionDuration :: NominalDiffTime
  }
