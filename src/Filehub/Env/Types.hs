module Filehub.Env.Types where
import Log (LogLevel)
import Log.Logger (Logger)
-- import Filehub.Target.Types (Target)


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
