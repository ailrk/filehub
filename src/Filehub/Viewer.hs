module Filehub.Viewer
  ( isResource
  , takeResourceFiles
  , initViewer
  )
  where

import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Text
import Effectful ((:>), Eff, IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.FileSystem
import Effectful.Log (Log)
import Effectful.Reader.Dynamic (Reader)
import Filehub.ClientPath (fromClientPath, toClientPath)
import Filehub.Env (Env(..))
import Filehub.Session qualified as Session
import Filehub.Error (FilehubError(..))
import Filehub.Mime (isMime)
import Filehub.Sort (sortFiles)
import Filehub.Storage (Storage(..), getStorage)
import Filehub.Types ( File(..), ClientPath(..), SessionId, FilehubEvent(..), Resource (..), RawClientPath (..) )
import Lens.Micro.Platform ()
import Network.Mime (MimeType)
import System.FilePath (takeDirectory)
import Data.String.Interpolate (i)


isResource :: MimeType -> Bool
isResource s = any (s `isMime`)  ["image", "video", "audio"]


takeResourceFiles :: [File] -> [File]
takeResourceFiles = filter (isResource . (.mimetype))


toResource :: FilePath -> File -> Resource
toResource root f =
  Resource
    { url = let ClientPath path = toClientPath root f.path -- encode path url
             in RawClientPath [i|/serve?file=#{path}|]
                                              , mimetype = Text.decodeUtf8 f.mimetype
    }


initViewer :: (Reader Env :> es, Log :> es, Error FilehubError :> es, IOE :> es, FileSystem :> es)
           => SessionId -> FilePath -> ClientPath -> Eff es FilehubEvent
initViewer sessionId root clientPath = do
  storage <- getStorage sessionId
  let filePath = fromClientPath root clientPath
  let dir      = takeDirectory filePath
  order <- Session.getSortFileBy sessionId
  files <- takeResourceFiles . sortFiles order <$> (storage.ls dir)
  let idx       = fromMaybe 0 $ List.elemIndex filePath (fmap (.path) files)
  let resources = fmap (toResource root) files
  pure $ ViewerInited resources idx
