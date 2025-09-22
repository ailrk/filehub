module Filehub.Storage.Error
  ( mapError
  , withStorageError
  )
  where

import Data.Generics.Labels ()
import Effectful (Eff, Eff, (:>))
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)
import Effectful.Error.Dynamic (throwError, Error, runErrorNoCallStack)
import Filehub.Error
import Storage.Error (StorageError)
import Storage.Error qualified as StorageError


mapError :: StorageError -> FilehubError
mapError = \case
  StorageError.InvalidDir t  -> FilehubError InvalidDir t
  StorageError.FileExists t  -> FilehubError FileExists t
  StorageError.TargetError t -> FilehubError TargetError t
  StorageError.CopyError t   -> FilehubError CopyError t
  StorageError.WriteError t  -> FilehubError WriteError t


withStorageError :: (Error FilehubError :> es) => Eff (Error StorageError : es) b -> Eff es b
withStorageError action = runErrorNoCallStack action >>= either (\err -> throwError (mapError err)) pure
