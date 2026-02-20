module Filehub.Session
  ( setCurrentDir
  )
  where

import Data.Generics.Labels ()
import Filehub.Types
import Lens.Micro.Platform ()
import Prelude hiding (elem, readFile)
import Filehub.Monad (Filehub)
import Data.ClientPath (AbsPath)


setCurrentDir :: SessionId -> AbsPath -> Filehub ()
