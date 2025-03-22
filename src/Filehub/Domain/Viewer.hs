module Filehub.Domain.Viewer
  ( isResource
  , takeResourceFiles
  , initViewer
  )
  where

import Effectful.FileSystem
import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE)
import Effectful.Error.Dynamic (throwError, Error)
import Control.Monad (when)
import Filehub.Env (Env(..))
import Filehub.Env qualified as Env
import Lens.Micro.Platform ()
import Data.Text qualified as Text
import Data.List qualified as List
import System.FilePath (takeDirectory)
import Network.Mime (MimeType)
import Data.Text.Encoding qualified as Text
import Data.Maybe (fromMaybe)
import Filehub.Domain.Types (File(..), FilehubError (..), ClientPath(..), Resource(..), Viewer(..))
import Filehub.Domain.File (isDirectory, lsDir, sortFiles)
import Filehub.Domain.ClientPath (fromClientPath, toClientPath)
import Filehub.Domain.Mime (isMime)
import Filehub.Types (SessionId)


isResource :: MimeType -> Bool
isResource s = any (s `isMime`)  ["image", "video", "audio"]


takeResourceFiles :: [File] -> [File]
takeResourceFiles = filter (isResource . (.mimetype))


initViewer :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  FileSystem :> es) => SessionId -> FilePath -> ClientPath -> Eff es Viewer
initViewer sessionId root clientPath = do
  let filePath = fromClientPath root clientPath
  let dir = takeDirectory filePath
  isDir <- isDirectory dir
  when (not isDir) (throwError InvalidDir)
  order <- Env.getSortFileBy sessionId >>= maybe (throwError InvalidSession) pure
  files <- takeResourceFiles . sortFiles order <$> lsDir dir
  let idx = fromMaybe 0 $ List.elemIndex filePath (fmap (.path) files)
  let toResource f =
        Resource
          { url = Text.pack . (.unClientPath) . toClientPath root $ f.path
          , mimetype = Text.decodeUtf8  f.mimetype
          }
  let resources = toResource <$> files
  pure $ InitViewer resources idx
