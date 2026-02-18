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
import Data.ClientPath (AbsPath)


getRoot       :: SessionId -> Filehub AbsPath
getCurrentDir :: SessionId -> Filehub AbsPath
setCurrentDir :: SessionId -> AbsPath -> Filehub ()
currentTarget :: SessionId -> Filehub TargetView
