module Filehub.Session where

import Data.Generics.Labels ()
import Effectful (Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Log (Log)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Error (FilehubError (..))
import Filehub.Types
import Filehub.Session.Types (TargetView)
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude hiding (readFile)


getRoot       :: (Reader Env  :> es,  Error FilehubError :> es,  IOE :> es, Log :> es) => SessionId -> Eff es FilePath
getCurrentDir :: (Reader Env  :> es,  Error FilehubError :> es,  IOE :> es, Log :> es) => SessionId -> Eff es FilePath
setCurrentDir :: (Reader Env  :> es,  IOE :> es) => SessionId -> FilePath -> Eff es ()
currentTarget :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es TargetView
