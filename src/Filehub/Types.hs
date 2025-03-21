module Filehub.Types where


import Effectful.Concurrent.STM
import Prelude hiding (readFile, writeFile)
import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Prelude hiding (readFile, writeFile)
import Filehub.Domain.Types (File, Theme, SortFileBy)


data Env = Env
  { root :: !FilePath
  , port :: !Int
  , rootTree :: TVar File
  , currentDir :: TVar FilePath
  , sortFileBy :: TVar SortFileBy
  , dataDir :: !FilePath
  , theme :: Theme
  }
