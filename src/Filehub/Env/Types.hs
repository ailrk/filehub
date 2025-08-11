module Filehub.Env.Types (Env(..)) where

import Data.Time (NominalDiffTime)
import Filehub.Theme (Theme)
import Filehub.SessionPool.Types (SessionPool)
import Filehub.Target.Types (Target)
import Filehub.User (UserDB)
import Log (Logger, LogLevel)

data Env = Env
  { port :: !Int
  , theme :: Theme
  , sessionPool :: SessionPool
  , sessionDuration :: NominalDiffTime
  , targets :: [Target]
  , readOnly :: Bool
  , logger :: Logger
  , logLevel :: LogLevel
  , userDB :: UserDB
  , noLogin :: Bool
  }
