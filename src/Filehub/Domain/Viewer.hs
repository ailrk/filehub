{-# LANGUAGE NamedFieldPuns #-}

module Filehub.Domain.Viewer
  ( Viewer(..)
  , Resource(..)
  , isResource
  , takeResourceFiles
  , initViewer
  )
  where

import Effectful.FileSystem
import Effectful.Log (Log, logAttention)
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
import Filehub.Domain.Types (File(..), ClientPath(..))
import Filehub.Error (FilehubError(..))
import Filehub.Domain (sortFiles)
import Filehub.Domain.ClientPath (fromClientPath, toClientPath)
import Filehub.Domain.Mime (isMime)
import Filehub.Types (SessionId)
import Filehub.Storage (isDirectory, lsDir, runStorage)
import Lens.Micro
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Text.Lazy.Encoding qualified as LText
import Data.Text (Text)
import Servant (ToHttpApiData(..))


data Resource = Resource
  { url :: Text
  , mimetype :: Text
  }
  deriving (Show, Eq)


instance ToJSON Resource where
  toJSON (Resource { url, mimetype }) =
    Aeson.object
      [ "url" .= toJSON url
      , "mimetype" .= mimetype
      ]


data Viewer = InitViewer [Resource] Int -- Update image list and show the viewer
  deriving (Show)


instance ToJSON Viewer where
  toJSON (InitViewer res index) =
    Aeson.object
      [ "InitViewer" .= Aeson.object
          [ "resources" .= toJSON res
          , "index" .= toJSON index
          ]
      ]


instance ToHttpApiData Viewer where
  toUrlPiece v = (v & Aeson.encode & LText.decodeUtf8) ^. strict


isResource :: MimeType -> Bool
isResource s = any (s `isMime`)  ["image", "video", "audio"]


takeResourceFiles :: [File] -> [File]
takeResourceFiles = filter (isResource . (.mimetype))


initViewer :: (Reader Env :> es, Log :> es, Error FilehubError :> es, IOE :> es, FileSystem :> es)
           => SessionId -> FilePath -> ClientPath -> Eff es Viewer
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
          { url = Text.pack . (.unClientPath) . toClientPath root $ f.path
          , mimetype = Text.decodeUtf8 f.mimetype
          }
  let resources = fmap toResource files
  pure $ InitViewer resources idx
