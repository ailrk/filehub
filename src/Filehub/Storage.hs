{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage
  ( Storage(..)
  , getFile
  , isDirectory
  , readFileContent
  , newFolder
  , newFile
  , writeFile
  , deleteFile
  , lsDir
  , changeDir
  , lsCurrentDir
  , upload
  , download
  , runStorage
  )
  where


import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Effectful ( Eff, (:>), Eff )
import Effectful.Dispatch.Dynamic (send)
import Filehub.Env qualified as Env
import Filehub.Target (TargetView(..))
import Filehub.Storage.Context qualified as Storage
import Filehub.Storage.Effect (Storage (..))
import Filehub.Storage.File qualified as File
import Filehub.Storage.S3 qualified as S3
import Filehub.Types (File(..), ClientPath, SessionId, Target(..))
import Lens.Micro.Platform ()
import Prelude hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem)


getFile :: Storage :> es => FilePath -> Eff es File
getFile path = send (GetFile path)


isDirectory :: Storage :> es => FilePath -> Eff es Bool
isDirectory path = send (IsDirectory path)


readFileContent :: Storage :> es => File -> Eff es LBS.ByteString
readFileContent file = send (ReadFileContent file)


newFolder :: Storage :> es => FilePath -> Eff es ()
newFolder path = send (NewFolder path)


newFile :: Storage :> es => FilePath -> Eff es ()
newFile path = send (NewFile path)


writeFile :: Storage :> es => FilePath -> LBS.ByteString -> Eff es ()
writeFile path bytes = send (WriteFile path bytes)


deleteFile :: Storage :> es => FilePath -> Eff es ()
deleteFile path = send (DeleteFile path)


lsDir :: Storage :> es => FilePath -> Eff es [File]
lsDir path = send (LsDir path)


changeDir :: Storage :> es => FilePath -> Eff es ()
changeDir path = send (ChangeDir path)


lsCurrentDir :: Storage :> es => Eff es [File]
lsCurrentDir = send LsCurrentDir


upload :: Storage :> es => MultipartData Mem -> Eff es ()
upload multipart = send (Upload multipart)


download :: Storage :> es => ClientPath -> Eff es LBS.ByteString
download clientPath = send (Download clientPath )


runStorage :: Storage.Context es => SessionId -> Eff (Storage : es) a -> Eff es a
runStorage sessionId eff = do
  TargetView target _ _ <- Env.currentTarget sessionId
  case target of
    S3Target _ -> S3.runStorageS3 sessionId eff
    FileTarget _ -> File.runStorageFile sessionId eff
