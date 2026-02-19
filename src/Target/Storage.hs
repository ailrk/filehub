module Target.Storage where

import Conduit (ResourceT)
import Data.File (FileInfo, FileWithContent)
import Data.ByteString (ByteString)
import Data.ClientPath (ClientPath, AbsPath)
import Data.Conduit (ConduitT)
import Data.Generics.Labels ()
import Lens.Micro.Platform ()
import Prelude hiding (readFile, writeFile)
import Servant.Multipart ( Mem, FileData )


data Storage m t = Storage
  { get         :: AbsPath -> m (Maybe FileInfo)

  , read        :: FileInfo -> m ByteString

  , readStream  :: FileInfo -> m (ConduitT () ByteString (ResourceT IO) ())

  , write       :: FileWithContent -> m ()

  , mv          :: [(AbsPath, AbsPath)] -> m ()

  , rename      :: AbsPath -> AbsPath -> m ()

  , delete      :: AbsPath -> m ()

  , new         :: AbsPath -> m ()

  , newFolder   :: AbsPath -> m ()

  , ls          :: AbsPath -> m [FileInfo]

  , cd          :: AbsPath -> m ()

  , lsCwd       :: m [FileInfo]

  , upload      :: FileData Mem -> m ()

  , download    :: ClientPath
                -> m (ConduitT () ByteString (ResourceT IO) ())

  , isDirectory :: AbsPath -> m Bool
  }
