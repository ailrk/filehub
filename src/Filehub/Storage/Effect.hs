{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage.Effect (Storage(..)) where

import Filehub.Domain.Types
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
import Filehub.Types ( SessionId, SessionId )
import Prelude hiding (readFile, writeFile)


data Storage :: Effect where
  GetFile :: SessionId -> FilePath -> Storage m File
  IsDirectory :: SessionId -> FilePath -> Storage m Bool
  ReadFileContent :: SessionId -> File -> Storage m LBS.ByteString
  NewFolder :: SessionId -> FilePath -> Storage m ()
  NewFile :: SessionId -> FilePath -> Storage m ()
  WriteFile :: SessionId -> FilePath -> LBS.ByteString -> Storage m ()
  DeleteFile :: SessionId -> FilePath -> Storage m ()
  LsDir :: SessionId -> FilePath -> Storage m [File]
  ChangeDir :: SessionId -> FilePath -> Storage m ()
  LsCurrentDir :: SessionId -> Storage m [File]
  Upload :: SessionId -> MultipartData Mem -> Storage m ()
  Download :: SessionId -> ClientPath -> Storage m LBS.ByteString

type instance DispatchOf Storage = Dynamic
