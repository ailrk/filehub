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
  GetFile :: FilePath -> Storage m File
  IsDirectory :: FilePath -> Storage m Bool
  ReadFileContent :: File -> Storage m LBS.ByteString
  NewFolder :: FilePath -> Storage m ()
  NewFile :: FilePath -> Storage m ()
  WriteFile :: FilePath -> LBS.ByteString -> Storage m ()
  DeleteFile :: FilePath -> Storage m ()
  LsDir :: FilePath -> Storage m [File]
  ChangeDir :: FilePath -> Storage m ()
  LsCurrentDir :: Storage m [File]
  Upload :: MultipartData Mem -> Storage m ()
  Download :: ClientPath -> Storage m LBS.ByteString

type instance DispatchOf Storage = Dynamic
