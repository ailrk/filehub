module Filehub.Session
  ( getRoot
  , getCurrentDir
  , setCurrentDir
  , currentTarget
  )
  where

import Data.Generics.Labels ()
import Filehub.Types
import Filehub.Session.Types (TargetView)
import Lens.Micro.Platform ()
import Prelude hiding (elem, readFile)
import Filehub.Monad (Filehub)


getRoot       :: SessionId -> Filehub FilePath
getCurrentDir :: SessionId -> Filehub FilePath
setCurrentDir :: SessionId -> FilePath -> Filehub ()
currentTarget :: SessionId -> Filehub TargetView
