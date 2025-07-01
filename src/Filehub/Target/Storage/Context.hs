{-# LANGUAGE ConstraintKinds #-}

module Filehub.Target.Storage.Context (Context) where

import Filehub.Error (FilehubError(..))
import Effectful ((:>), IOE)
import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Prelude hiding (readFile, writeFile)
import Filehub.Env.Types ( Env )
import Effectful.FileSystem
import Effectful.Reader.Dynamic (Reader)
import Effectful.Error.Dynamic (Error)
import Effectful.Log
import Prelude hiding (readFile, writeFile)


type Context es =
     ( Reader Env :> es
     , FileSystem :> es
     , Log :> es
     , IOE :> es
     , Error FilehubError :> es
     )
