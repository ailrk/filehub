{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Types where


import Effectful.Concurrent.STM
import Filehub.Domain.Types (Theme, SortFileBy)
import Data.UUID (UUID)
import Data.Time (UTCTime)
import Data.HashTable.IO (BasicHashTable)
import Data.Hashable (Hashable)


newtype SessionId = SessionId UUID
  deriving (Show, Eq, Ord, Hashable)


data Session = Session
  { sessionId :: SessionId
  , expireDate :: UTCTime
  , currentDir :: FilePath
  , sortedFileBy :: SortFileBy
  }
  deriving (Eq)


newtype SessionPool = SessionPool (BasicHashTable SessionId Session)


data Env = Env
  { root :: !FilePath
  , port :: !Int
  , currentDir :: TVar FilePath
  , sortFileBy :: TVar SortFileBy
  , dataDir :: !FilePath
  , theme :: Theme
  , sessionPool :: SessionPool
  }
