module Target.Storage where

import Conduit (ResourceT)
import Data.File (FileInfo, FileWithContent)
import Data.ByteString (ByteString)
import Data.ClientPath (ClientPath)
import Data.Conduit (ConduitT)
import Data.Generics.Labels ()
import Lens.Micro.Platform ()
import Prelude hiding (readFile, writeFile)
import Servant.Multipart ( Mem, FileData )


data Storage m = Storage
  { get         :: FilePath -> m (Maybe FileInfo)

  , read        :: FileInfo -> m ByteString

  , readStream  :: FileInfo -> m (ConduitT () ByteString (ResourceT IO) ())

  , write       :: FilePath -> FileWithContent -> m ()

  , mv          :: [(FilePath, FilePath)] -> m ()

  , delete      :: FilePath -> m ()

  , new         :: FilePath -> m ()

  , newFolder   :: FilePath -> m ()

  , ls          :: FilePath -> m [FileInfo]

  , cd          :: FilePath -> m ()

  , lsCwd       :: m [FileInfo]

  , upload      :: FileData Mem -> m ()

  , download    :: ClientPath
                -> m (ConduitT () ByteString (ResourceT IO) ())

  , isDirectory :: FilePath -> m Bool
  }
