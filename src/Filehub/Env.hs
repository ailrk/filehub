module Filehub.Env where
import UnliftIO.IORef
import {-# SOURCE#-} Filehub.Domain (File)


data Env = Env
  { root :: !FilePath
  , port :: !Int
  , configFile :: !FilePath
  , rootTree :: IORef File
  , currentDir :: IORef FilePath
  , dataDir :: !FilePath
  }
