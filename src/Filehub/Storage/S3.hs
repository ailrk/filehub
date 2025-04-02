{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage.S3 (runStorageS3) where

import Effectful.Dispatch.Dynamic (interpret)
import Effectful ( Eff, Eff )
import Prelude hiding (readFile, writeFile)
import Data.Generics.Labels ()
import Prelude hiding (readFile, writeFile)
import Filehub.Storage.Effect (Storage (..))
import Filehub.Storage.Context qualified as Storage


runStorageS3 :: Storage.Context es => Eff (Storage : es) a -> Eff es a
runStorageS3 = interpret $ \_ -> undefined
