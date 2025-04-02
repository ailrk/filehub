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

import Effectful ( Eff, (:>), Eff )
import Lens.Micro.Platform ()
import Prelude hiding (readFile, writeFile)
import Data.Generics.Labels ()
import Filehub.Storage.File qualified as File
import Effectful.Dispatch.Dynamic (send)
import Prelude hiding (readFile, writeFile)
import Filehub.Domain.Types (File(..), ClientPath)
import Filehub.Types (SessionId)
import Servant.Multipart (MultipartData(..), Mem)
import Data.ByteString.Lazy qualified as LBS
import Filehub.Storage.Effect (Storage (..))
import Filehub.Storage.Context qualified as Storage


getFile :: Storage :> es => FilePath -> Eff es File
getFile path = send (GetFile path)


isDirectory :: Storage :> es => FilePath -> Eff es Bool
isDirectory path = send (IsDirectory path)


readFileContent :: Storage :> es => File -> Eff es LBS.ByteString
readFileContent file = send (ReadFileContent file)


newFolder :: Storage :> es => SessionId -> FilePath -> Eff es ()
newFolder sessionId path = send (NewFolder sessionId path)


newFile :: Storage :> es => SessionId -> FilePath -> Eff es ()
newFile sessionId path = send (NewFile sessionId path)


writeFile :: Storage :> es => SessionId -> FilePath -> LBS.ByteString -> Eff es ()
writeFile sessionId path bytes = send (WriteFile sessionId path bytes)


deleteFile :: Storage :> es => SessionId -> FilePath -> Eff es ()
deleteFile sessionId path = send (DeleteFile sessionId path)


lsDir :: Storage :> es => FilePath -> Eff es [File]
lsDir path = send (LsDir path)


changeDir :: Storage :> es => SessionId -> FilePath -> Eff es ()
changeDir sessionId path = send (ChangeDir sessionId path)


lsCurrentDir :: Storage :> es => SessionId -> Eff es [File]
lsCurrentDir sessionId = send (LsCurrentDir sessionId)


upload :: Storage :> es => SessionId -> MultipartData Mem -> Eff es ()
upload sessionId multipart = send (Upload sessionId multipart)


download :: Storage :> es => SessionId -> ClientPath -> Eff es LBS.ByteString
download sessionId clientPath = send (Download sessionId clientPath )


runStorage :: Storage.Context es => Eff (Storage : es) a -> Eff es a
runStorage eff = do
    File.runStorageFile eff
