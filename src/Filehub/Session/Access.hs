module Filehub.Session.Access where
import Data.ClientPath (AbsPath)
import Filehub.Session (Storage)
import Filehub.Monad (Filehub)



data SessionView = SessionView
  { currentDir :: AbsPath
  , storage :: Storage Filehub

  }

