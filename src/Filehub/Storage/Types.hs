module Filehub.Storage.Types where

import Lens.Micro.Platform ()
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Servant.Multipart
    ( MultipartData(..), Mem, MultipartData(..), Mem )
import Prelude hiding (readFile, writeFile)
import Data.Conduit (ConduitT)
import Conduit (ResourceT)
import Data.ByteString (ByteString)
import Filehub.File (File)
import Filehub.ClientPath (ClientPath)


data Storage m = Storage
  { get :: FilePath -> m File
  , read :: File -> m LBS.ByteString
  , readStream :: File -> m (ConduitT () ByteString (ResourceT IO) ())
  , write :: FilePath -> LBS.ByteString -> m ()
  , delete :: FilePath -> m ()
  , new :: FilePath -> m ()
  , newFolder :: FilePath -> m ()
  , ls :: FilePath -> m [File]
  , cd :: FilePath -> m ()
  , lsCwd :: m [File]
  , upload :: MultipartData Mem -> m ()
  , download :: ClientPath -> m (ConduitT () ByteString (ResourceT IO) ())
  , isDirectory :: FilePath -> m Bool
  }
