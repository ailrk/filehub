module Filehub.Env where
import {-# SOURCE#-} Filehub.Domain (File)
import Effectful.Concurrent.STM (TVar)


data Env = Env
  { root :: !FilePath
  , port :: !Int
  , configFile :: !FilePath
  , rootTree :: TVar File
  , currentDir :: TVar FilePath
  , dataDir :: !FilePath
  }
