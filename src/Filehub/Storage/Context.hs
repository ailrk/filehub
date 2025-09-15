{-# LANGUAGE ConstraintKinds #-}
module Filehub.Storage.Context (Context) where

import Filehub.Error (FilehubError(..))
import Effectful ((:>), IOE)
import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Prelude hiding (readFile, writeFile)
import Filehub.Env ( Env )
import Effectful.FileSystem
import Effectful.Reader.Dynamic (Reader)
import Effectful.Error.Dynamic (Error)
import Effectful.Log
import Prelude hiding (readFile, writeFile)
import Filehub.Effectful.LockManager (LockManager)
import Filehub.Effectful.Cache (Cache)


type Context es =
     ( Reader Env         :> es
     , FileSystem         :> es
     , Log                :> es
     , IOE                :> es
     , Cache              :> es
     , LockManager        :> es
     , Error FilehubError :> es
     )
