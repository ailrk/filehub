{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage
  ( Storage(..)
  , get
  , read
  , write
  , delete
  , new
  , newFolder
  , ls
  , cd
  , lsCwd
  , upload
  , download
  , isDirectory
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
import Prelude hiding (read, readFile, writeFile)
import Prelude hiding (read, readFile, writeFile)
import Servant.Multipart (MultipartData(..), Mem)


get :: Storage :> es => FilePath -> Eff es File
get path = send (Get path)


isDirectory :: Storage :> es => FilePath -> Eff es Bool
isDirectory path = send (IsDirectory path)


read :: Storage :> es => File -> Eff es LBS.ByteString
read file = send (Read file)


newFolder :: Storage :> es => FilePath -> Eff es ()
newFolder path = send (NewFolder path)


new :: Storage :> es => FilePath -> Eff es ()
new path = send (New path)


write :: Storage :> es => FilePath -> LBS.ByteString -> Eff es ()
write path bytes = send (Write path bytes)


delete :: Storage :> es => FilePath -> Eff es ()
delete path = send (Delete path)


ls :: Storage :> es => FilePath -> Eff es [File]
ls path = send (Ls path)


cd :: Storage :> es => FilePath -> Eff es ()
cd path = send (Cd path)


lsCwd :: Storage :> es => Eff es [File]
lsCwd = send LsCwd


upload :: Storage :> es => MultipartData Mem -> Eff es ()
upload multipart = send (Upload multipart)


download :: Storage :> es => ClientPath -> Eff es LBS.ByteString
download clientPath = send (Download clientPath)


runStorage :: Storage.Context es => SessionId -> Eff (Storage : es) a -> Eff es a
runStorage sessionId eff = do
  TargetView target _ _ <- Env.currentTarget sessionId
  case target of
    S3Target _ -> S3.runStorageS3 sessionId eff
    FileTarget _ -> File.runStorageFile sessionId eff
