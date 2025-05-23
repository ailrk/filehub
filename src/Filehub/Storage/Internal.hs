module Filehub.Storage.Internal where

import Filehub.Types
    ( File,
      File(..),
      ClientPath,
      File(..),
      ClientPath )
import Lens.Micro.Platform ()
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Servant.Multipart
    ( MultipartData(..), Mem, MultipartData(..), Mem )
import Prelude hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile)


data Storage m = Storage
  { get :: FilePath -> m File
  , read :: File -> m LBS.ByteString
  , write :: FilePath -> LBS.ByteString -> m ()
  , delete :: FilePath -> m ()
  , new :: FilePath -> m ()
  , newFolder :: FilePath -> m ()
  , ls :: FilePath -> m [File]
  , cd :: FilePath -> m ()
  , lsCwd :: m [File]
  , upload :: MultipartData Mem -> m ()
  , download :: ClientPath -> m LBS.ByteString
  , isDirectory :: FilePath -> m Bool
  }
