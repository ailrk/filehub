module Filehub.Env where
import {-# SOURCE #-} Filehub.Domain (File, Theme, SortFileBy)
import Effectful.Concurrent.STM (TVar)


data Env = Env
  { root :: !FilePath
  , port :: !Int
  , rootTree :: TVar File
  , currentDir :: TVar FilePath
  , sortFileBy :: TVar SortFileBy
  , dataDir :: !FilePath
  , theme :: Theme
  }
