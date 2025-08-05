module Filehub.Env.Types (Env(..)) where

import Filehub.Theme (Theme)
import Filehub.SessionPool.Types (SessionPool)
import Data.Time (NominalDiffTime)
import Filehub.Target.Types (Target)
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
  }
