module Filehub.Storage.Types where

import Conduit (ResourceT)
import Data.File (FileInfo)
import Data.ByteString (ByteString)
import Data.ClientPath (ClientPath)
import Data.Conduit (ConduitT)
import Data.Generics.Labels ()
import Lens.Micro.Platform ()
import Prelude hiding (readFile, writeFile)
import Servant.Multipart ( MultipartData(..), Mem, MultipartData(..), Mem )


data Storage m = Storage
  { get         :: FilePath -> m FileInfo

  , read        :: FileInfo -> m ByteString

  , readStream  :: FileInfo -> m (ConduitT () ByteString (ResourceT IO) ())

  , write       :: FilePath -> ByteString -> m ()

  , writeStream :: FilePath                                 -- the path to write to
                -> ConduitT () ByteString (ResourceT IO) () -- file source as a conduit
                -> Maybe Integer                            -- optional file size
                -> m ()

  , mv          :: [(FilePath, FilePath)] -> m ()

  , delete      :: FilePath -> m ()

  , new         :: FilePath -> m ()

  , newFolder   :: FilePath -> m ()

  , ls          :: FilePath -> m [FileInfo]

  , cd          :: FilePath -> m ()

  , lsCwd       :: m [FileInfo]

  , upload      :: MultipartData Mem -> m ()

  , download    :: ClientPath
                -> m (ConduitT () ByteString (ResourceT IO) ())

  , isDirectory :: FilePath -> m Bool
  }
