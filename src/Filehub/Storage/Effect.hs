{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage.Effect (Storage(..)) where

import Filehub.Types
    ( File,
      File(..),
      ClientPath,
      File(..),
      ClientPath )
import Effectful
    ( Effect,
      DispatchOf,
      Dispatch(..) )
import Lens.Micro.Platform ()
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Servant.Multipart
    ( MultipartData(..), Mem, MultipartData(..), Mem )
import Prelude hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile)


data Storage :: Effect where
  Get :: FilePath -> Storage m File
  Read :: File -> Storage m LBS.ByteString
  Write :: FilePath -> LBS.ByteString -> Storage m ()
  Delete :: FilePath -> Storage m ()
  New :: FilePath -> Storage m ()
  NewFolder :: FilePath -> Storage m ()
  Ls :: FilePath -> Storage m [File]
  Cd :: FilePath -> Storage m ()
  LsCwd :: Storage m [File]
  Upload :: MultipartData Mem -> Storage m ()
  Download :: ClientPath -> Storage m LBS.ByteString
  IsDirectory :: FilePath -> Storage m Bool

type instance DispatchOf Storage = Dynamic
