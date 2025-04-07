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
import Filehub.Domain.Types (File)

getFile :: Storage.Context es => FilePath -> Eff es File
getFile path = undefined

runStorageS3 :: Storage.Context es => Eff (Storage : es) a -> Eff es a
runStorageS3 = interpret $ \_ -> \case
  GetFile path -> getFile path
  IsDirectory path -> undefined
  ReadFileContent file -> undefined
  NewFolder sessionId path -> undefined
  NewFile sessionId path -> undefined
  WriteFile sessionId path bytes -> undefined
  DeleteFile sessionId path -> undefined
  LsDir path -> undefined
  ChangeDir sessionId path -> undefined
  LsCurrentDir sessionId -> undefined
  Upload sessionId multipart -> undefined
  Download sessionId clientPath -> undefined
