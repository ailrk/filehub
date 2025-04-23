module Filehub.Viewer
  ( isResource
  , takeResourceFiles
  , initViewer
  )
  where

import Control.Monad (when)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Text
import Effectful ((:>), Eff, IOE)
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.FileSystem
import Effectful.Log (Log, logAttention)
import Effectful.Reader.Dynamic (Reader)
import Filehub.ClientPath (fromClientPath, toRawClientPath)
import Filehub.Env (Env(..))
import Filehub.Env qualified as Env
import Filehub.Error (FilehubError(..))
import Filehub.Mime (isMime)
import Filehub.Sort (sortFiles)
import Filehub.Storage (isDirectory, lsDir, runStorage)
import Filehub.Types ( File(..), ClientPath(..), SessionId, FilehubEvent(..), Resource (..) )
import Lens.Micro.Platform ()
import Network.Mime (MimeType)
import System.FilePath (takeDirectory)


isResource :: MimeType -> Bool
isResource s = any (s `isMime`)  ["image", "video", "audio"]


takeResourceFiles :: [File] -> [File]
takeResourceFiles = filter (isResource . (.mimetype))


initViewer :: (Reader Env :> es, Log :> es, Error FilehubError :> es, IOE :> es, FileSystem :> es)
           => SessionId -> FilePath -> ClientPath -> Eff es FilehubEvent
initViewer sessionId root clientPath = do
  let filePath = fromClientPath root clientPath
  let dir = takeDirectory filePath
  isDir <- runStorage sessionId $ isDirectory dir
  when (not isDir) $ do
    logAttention "[initViewer] invalid dir" dir
    throwError InvalidDir
  order <- Env.getSortFileBy sessionId
  files <- takeResourceFiles . sortFiles order <$> runStorage sessionId (lsDir dir)
  let idx = fromMaybe 0 $ List.elemIndex filePath (fmap (.path) files)
  let toResource f =
        Resource
          { url = toRawClientPath root f.path
          , mimetype = Text.decodeUtf8 f.mimetype
          }
  let resources = fmap toResource files
  pure $ ViewerInited resources idx
