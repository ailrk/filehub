module Filehub.Server.InitViewer (initViewer) where

import Data.ClientPath (ClientPath (..), AbsPath)
import Data.ClientPath qualified as ClientPath
import Data.File (File(..), FileInfo)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text.Encoding qualified as Text
import Filehub.Handler (ConfirmLogin)
import Filehub.Orphan ()
import Filehub.Server.Internal (withQueryParam)
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Filehub.Sort qualified as Sort
import Filehub.Types (Resource (..), FilehubEvent (..))
import Network.Mime (MimeType)
import Network.Mime.Extended (isMime)
import Prelude hiding (init, readFile)
import Servant (addHeader, Headers, Header, NoContent(..))
import System.FilePath (takeDirectory)
import Filehub.Monad (Filehub)
import Data.Coerce (coerce)


initViewer :: SessionId -> ConfirmLogin -> Maybe ClientPath
           -> Filehub (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
initViewer sessionId _ mClientPath = do
  clientPath <- withQueryParam mClientPath
  root       <- Session.getRoot sessionId
  payload    <- initViewer' root clientPath
  pure $ addHeader payload NoContent
  where
    initViewer' root clientPath = do
      Session.withStorage sessionId \storage -> do
        let filePath  =  ClientPath.fromClientPath root clientPath
        let dir       =  coerce takeDirectory filePath
        order         <- Session.getSortFileBy sessionId
        files         <- takeResourceFiles . Sort.sortFiles order <$> (storage.ls dir)
        let idx       =  fromMaybe 0 $ List.elemIndex filePath (fmap (.path) files)
        let resources =  fmap (toResource root) files
        pure $ ViewerInited resources idx

    isResource :: MimeType -> Bool
    isResource s = any (s `isMime`)  ["image", "video", "audio"]

    takeResourceFiles :: [FileInfo] -> [FileInfo]
    takeResourceFiles = filter (isResource . (.mimetype))

    toResource :: AbsPath -> FileInfo -> Resource
    toResource root f =
      Resource
        { url = let ClientPath path = ClientPath.toClientPath root f.path -- encode path url
                 in ClientPath.RawClientPath [i|/serve?file=#{path}|]
                                                  , mimetype = Text.decodeUtf8 f.mimetype
        }
