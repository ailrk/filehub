{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server (server) where

import Data.String.Interpolate (i)
import Data.Maybe (fromMaybe, isJust)
import Data.Foldable (forM_)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.Log (logAttention_)
import Effectful ( withRunInIO, MonadIO (liftIO) )
import Effectful.Error.Dynamic (throwError)
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Servant (errBody, Headers, Header, NoContent (..))
import Servant.Server.Generic (AsServerT)
import Control.Exception (SomeException)
import Servant ( addHeader, err500 )
import UnliftIO (catch)
import Control.Monad (when)
import System.FilePath ((</>), takeFileName)
import Text.Printf (printf)
import Filehub.Target qualified as Target
import Filehub.Types
    ( FilehubEvent (..))
import Filehub.Env (TargetView (..))
import Filehub.Env qualified as Env
import Filehub.Error ( withServerError, FilehubError(..), FilehubError(..), withServerError )
import Filehub.Monad ( Filehub )
import Filehub.Routes (Api (..))
import Filehub.Types
    ( SessionId(..),
      SessionId(..), Display (..))
import Filehub.Server.Desktop qualified as Server.Desktop
import Filehub.Server.Mobile qualified as Server.Mobile
import Filehub.Template.Internal qualified as Template
import Filehub.Template qualified as Template
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.ClientPath qualified as ClientPath
import Filehub.Selected qualified as Selected
import Filehub.Server.Internal (withQueryParam, clear, copy, paste)
import Filehub.Theme qualified as Theme
import Filehub.Types
    ( File(..),
      ClientPath(..),
      UpdatedFile(..),
      ClientPath(..),
      NewFile(..),
      NewFolder(..),
      SortFileBy(..),
      UpdatedFile(..),
      Theme(..),
      Selected (..))
import Filehub.Viewer qualified as Viewer
import Filehub.ControlPanel qualified as ControlPanel
import Data.ByteString.Char8 qualified as ByteString
import Filehub.Storage (getStorage, Storage(..))
import Data.ByteString (ByteString)
import Conduit (ConduitT, ResourceT)
import Data.Aeson (object, KeyValue (..), (.:), withObject)
import Data.ByteString.Lazy qualified as LBS
import Data.Aeson.Types (parseMaybe)


-- | Server definition
server :: Api (AsServerT Filehub)
server = Api
  { init = \sessionId res -> do
      Env.updateSession sessionId $
        \s -> s & #resolution .~ Just res
      clear sessionId
      index sessionId


  -- Only the top level index will load resources (js, css, etc) if display is valid.
  -- Whenver you need to re-render the index page, call the `index` free function instead,
  -- which only render the element #index.
  --
  -- `bootstrap` is used to query the device resolution before rendering anything. Once
  -- the session is bootstrapped, display information will be available for all subsequent
  -- requests.
  --
  -- The frontend js deletes the `display` cookie on `pageunload`, so the backend can
  -- start a full reload from the bootstrap stage.
  , index = \sessionId -> do
      display <- Env.getDisplay sessionId & withServerError
      manifest <- server.manifest
      let background
            = fromMaybe "#000000"
            $ flip parseMaybe manifest
            $ withObject "manifest"
            $ (.: "theme_color")

      clear sessionId
      case display of
        NoDisplay -> pure Template.bootstrap
        Desktop -> fmap (Template.withDefault display background) $ Server.Desktop.index sessionId
        Mobile -> fmap (Template.withDefault display background) $ Server.Mobile.index sessionId


  , cd = \sessionId mClientPath -> do
      clientPath <- withQueryParam mClientPath
      withServerError do
        root <- Env.getRoot sessionId
        storage <- getStorage sessionId
        storage.cd (ClientPath.fromClientPath root clientPath)
      view sessionId <&> addHeader DirChanged


  , newFile = \sessionId _ (NewFile path) -> do
      withServerError do
        storage <- getStorage sessionId
        storage.new (Text.unpack path)
      view sessionId


  , updateFile = \sessionId _ (UpdatedFile clientPath content) -> do
      let path = clientPath.unClientPath
      withServerError do
        storage <- getStorage sessionId
        storage.write path (Text.encodeUtf8 content ^. lazy)
      view sessionId


  , deleteFile = \sessionId _ mClientPath deleteSelected -> do
      withServerError do
        root <- Env.getRoot sessionId
        storage <- getStorage sessionId

        when (isJust mClientPath) do
          clientPath <- withQueryParam mClientPath
          let p = ClientPath.fromClientPath root clientPath
          storage.delete p

        when deleteSelected do
          allSelecteds <- Selected.allSelecteds sessionId
          forM_ allSelecteds $ \(target, selected) -> do
            Target.withTarget sessionId (Target.getTargetId target) do
              case selected of
                NoSelection -> pure ()
                Selected x xs -> do
                  forM_ (fmap (ClientPath.fromClientPath root) (x:xs)) $ \path -> do
                    storage.delete path
      clear sessionId
      count <- Selected.countSelected sessionId & withServerError
      addHeader count <$> index sessionId


  , newFolder = \sessionId _ (NewFolder path) -> do
      withServerError do
        storage <- getStorage sessionId
        storage.newFolder (Text.unpack path)
      view sessionId


  , newFileModal = \_ _ _ -> pure Template.Desktop.newFileModal


  , newFolderModal = \_ _ _ -> pure Template.Desktop.newFolderModal


  , fileDetailModal = Server.Desktop.fileDetailModal


  , editorModal = \sessionId mClientPath -> do
      display <- Env.getDisplay sessionId & withServerError
      case display of
        Mobile -> Server.Mobile.editorModal sessionId mClientPath
        Desktop -> Server.Desktop.editorModal sessionId mClientPath
        NoDisplay -> undefined


  , search = \sessionId searchWord -> withServerError do
      display <- Env.getDisplay sessionId
      storage <- getStorage sessionId
      TargetView target _ _ <- Env.currentTarget sessionId
      root <- Env.getRoot sessionId
      files <- storage.lsCwd
      order <- Env.getSortFileBy sessionId
      selected <- Selected.getSelected sessionId
      case display of
        Mobile -> pure $ Template.Mobile.search searchWord target root files selected order
        Desktop -> pure $ Template.Desktop.search searchWord target root files selected order
        NoDisplay -> undefined


  , sortTable = \sessionId order -> do
      Env.setSortFileBy sessionId (fromMaybe ByNameUp order)
      addHeader TableSorted <$> view sessionId


  , selectRows = \sessionId selected -> do
      case selected of
        NoSelection -> do
          logAttention_ [i|No selection: #{sessionId}|]
          throwError InvalidSelection & withServerError
        _ -> do
          Selected.setSelected sessionId selected
          count <- Selected.countSelected sessionId & withServerError
          addHeader count <$> controlPanel sessionId


  , upload = \sessionId _ multipart -> do
      withServerError do
        storage <- getStorage sessionId
        storage.upload multipart
      index sessionId


  , download = \sessionId mClientPath -> do
      clientPath@(ClientPath path) <- withQueryParam mClientPath
      bs <- withServerError do
        storage <- getStorage sessionId
        storage.download clientPath
      pure $ addHeader (printf "attachement; filename=%s" (takeFileName path)) bs


  , copy = \sessionId _ -> do
      copy sessionId
      controlPanel sessionId


  , paste = \sessionId _ -> do
      paste sessionId
      clear sessionId
      count <- Selected.countSelected sessionId & withServerError
      addHeader count <$> index sessionId


  , cancel = \sessionId -> do
      clear sessionId
      count <- Selected.countSelected sessionId & withServerError
      addHeader count <$> index sessionId


  , contextMenu = Server.Desktop.contextMenu


  , initViewer = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getCurrentDir sessionId
        payload <- Viewer.initViewer sessionId root clientPath
        pure $ addHeader payload NoContent


  , changeTarget = \sessionId mTargetId -> do
      savedTargetId <- withServerError do
        TargetView saved _ _ <- Target.currentTarget sessionId
        pure $ Target.getTargetId saved

      let restore = Env.changeCurrentTarget sessionId savedTargetId & withServerError
      targetId <- withQueryParam mTargetId
      Env.changeCurrentTarget sessionId targetId & withServerError

      html <- withRunInIO $ \unlift -> do
        unlift (index sessionId) `catch` \(_ :: SomeException) -> unlift do
          restore
          throwError (err500 { errBody = [i|Invalid target|]})

      pure $ addHeader TargetChanged html


  , themeCss = do
      theme <- Env.getTheme
      dir <- Env.getDataDir
      readFile $
        case theme of
          Dark -> dir </> "dark.css"
          Light -> dir </> "light.css"


  , serve = serve


  , manifest = do
      theme <- LBS.toStrict <$> server.themeCss
      let background1 = fromMaybe "" $ Theme.parse "--background2" theme
      let t = Text.pack
      pure $
        object
          [ "name"       .= t "FileHub"
          , "short_name" .= t "FileHub"
          , "start_url"  .= t "/"
          , "display"    .= t "standalone"
          , "theme_color" .= t background1
          , "background_color" .= t background1
          , "icons" .=
              [ object
                    [ "src"     .= t "/static/filehub/web-app-manifest-192x192.png"
                    , "sizes"   .= t "192x192"
                    , "type"    .= t "image/png"
                    , "purpose" .= t "maskable"
                    ]
              , object
                    [ "src"     .= t "/static/filehub/web-app-manifest-512x512.png"
                    , "sizes"   .= t "512x512"
                    , "type"    .= t "image/png"
                    , "purpose" .= t "maskable"
                    ]
              ]
          ]


  , favicon = do
      dir <- Env.getDataDir
      liftIO $ LBS.readFile $ dir </> "filehub/favicon.ico"


  , healthz = pure "ok"
  }



-- | index that reset the state
index :: SessionId -> Filehub (Html ())
index sessionId = do
  display <- Env.getDisplay sessionId & withServerError
  case display of
    NoDisplay -> pure Template.bootstrap
    Desktop -> Server.Desktop.index sessionId
    Mobile -> Server.Mobile.index sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  display <- Env.getDisplay sessionId & withServerError
  case display of
    Desktop -> Server.Desktop.view sessionId
    Mobile -> Server.Mobile.view sessionId
    NoDisplay -> Server.Mobile.view sessionId


controlPanel :: SessionId -> Filehub (Html ())
controlPanel sessionId = do
  display <- Env.getDisplay sessionId & withServerError
  case display of
    Desktop ->
      Template.Desktop.controlPanel
        <$> Env.getReadOnly
        <*> ControlPanel.getControlPanelState sessionId
          & withServerError
    Mobile ->
      Template.Mobile.controlPanel
        <$> Env.getReadOnly
        <*> ControlPanel.getControlPanelState sessionId
          & withServerError
    NoDisplay ->
      Template.Mobile.controlPanel
        <$> Env.getReadOnly
        <*> ControlPanel.getControlPanelState sessionId
          & withServerError


serve :: SessionId -> Maybe ClientPath -> Filehub (Headers '[ Header "Content-Type" String
                                                            , Header "Content-Disposition" String
                                                            ] (ConduitT () ByteString (ResourceT IO) ()))
serve sessionId mFile = do
  withServerError do
    storage <- getStorage sessionId
    root <- Env.getCurrentDir sessionId
    clientPath <- withQueryParam mFile
    let path = ClientPath.fromClientPath root clientPath
    file <- storage.get path
    conduit <- storage.readStream file
    pure
      $ addHeader (ByteString.unpack file.mimetype)
      $ addHeader (printf "inline; filename=%s" (takeFileName path))
      $ conduit
