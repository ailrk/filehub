{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server ( server) where

import Data.String.Interpolate (i)
import Data.Maybe (fromMaybe, isJust)
import Data.Foldable (forM_)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.Log (logAttention_)
import Effectful ( withRunInIO )
import Effectful.Error.Dynamic (throwError)
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Servant ( errBody)
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
import Filehub.Storage qualified as Storage
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
import Filehub.Server.Internal (withQueryParam, runStorage, clear)
import Filehub.Types
    ( ClientPath(..),
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
import Debug.Trace


-- | Server definition
server :: Api (AsServerT Filehub)
server = Api
  { init = \sessionId res -> do
      Env.updateSession sessionId $
        \s -> s & #resolution .~ Just res
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
      case display of
        NoDisplay -> pure Template.bootstrap
        Desktop -> fmap (Template.withDefault display) $ Server.Desktop.index sessionId
        Mobile -> fmap (Template.withDefault display) $ Server.Mobile.index sessionId


  , cd = \sessionId mClientPath -> do
      clientPath <- withQueryParam mClientPath
      root <- Env.getRoot sessionId & withServerError
      runStorage sessionId $ Storage.cd (ClientPath.fromClientPath root clientPath) & withServerError
      view sessionId <&> addHeader DirChanged


  , newFile = \sessionId _ (NewFile path) -> do
      runStorage sessionId $ Storage.new (Text.unpack path) & withServerError
      view sessionId


  , updateFile = \sessionId _ (UpdatedFile clientPath content) -> do
      let path = clientPath.unClientPath
      _ <- runStorage sessionId $ Storage.write path (Text.encodeUtf8 content ^. lazy)
      view sessionId


  , deleteFile = \sessionId _ mClientPath deleteSelected -> do
      withServerError do
        root <- Env.getRoot sessionId

        when (isJust mClientPath) do
          clientPath <- withQueryParam mClientPath
          let p = ClientPath.fromClientPath root clientPath
          runStorage sessionId  $ Storage.delete p

        when deleteSelected do
          allSelecteds <- Selected.allSelecteds sessionId
          forM_ allSelecteds $ \(target, selected) -> do
            Target.withTarget sessionId (Target.getTargetId target) do
              case selected of
                NoSelection -> pure ()
                Selected x xs -> do
                  forM_ (fmap (ClientPath.fromClientPath root) (x:xs)) $ \path -> do
                    runStorage sessionId  $ Storage.delete path
          clear sessionId
      index sessionId


  , newFolder = \sessionId _ (NewFolder path) -> do
      runStorage sessionId $ Storage.newFolder (Text.unpack path) & withServerError
      view sessionId


  , newFileModal = \_ _ _ -> pure Template.Desktop.newFileModal


  , newFolderModal = \_ _ _ -> pure Template.Desktop.newFolderModal


  , fileDetailModal = Server.Desktop.fileDetailModal


  , editorModal = Server.Desktop.editorModal


  , search = \sessionId searchWord -> do
      display <- Env.getDisplay sessionId & withServerError
      withServerError . runStorage sessionId $ do
        TargetView target _ _ <- Env.currentTarget sessionId & withServerError
        root <- Env.getRoot sessionId
        files <- Storage.lsCwd
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
      display <- Env.getDisplay sessionId & withServerError
      case selected of
        NoSelection -> do
          logAttention_ [i|No selection: #{sessionId}|]
          throwError InvalidSelection & withServerError
        _ -> do
          Selected.setSelected sessionId selected
          case display of
            Desktop ->
              Template.Desktop.controlPanel
                <$> Env.getReadOnly
                <*> ControlPanel.getControlPanelState sessionId & withServerError
            Mobile ->
              Template.Mobile.controlPanel
                <$> Env.getReadOnly
                <*> ControlPanel.getControlPanelState sessionId & withServerError
            NoDisplay -> undefined


  , upload = \sessionId _ multipart -> do
      runStorage sessionId $ Storage.upload multipart
      index sessionId


  , download = \sessionId mClientPath -> do
      clientPath@(ClientPath path) <- withQueryParam mClientPath
      bs <- runStorage sessionId $ Storage.download clientPath
      pure $ addHeader (printf "attachement; filename=%s" (takeFileName path)) bs


  , copy = Server.Desktop.copy


  , paste = Server.Desktop.paste


  , cancel = index


  , contextMenu = Server.Desktop.contextMenu


  , initViewer = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        payload <- Viewer.initViewer sessionId root clientPath
        pure $ addHeader payload mempty


  , changeTarget = \sessionId mTargetId -> do
      savedTargetId <- do
        TargetView saved _ _ <- Target.currentTarget sessionId & withServerError
        pure $ Target.getTargetId saved
      let restore = Env.changeCurrentTarget sessionId savedTargetId & withServerError
      targetId <- withQueryParam mTargetId
      Env.changeCurrentTarget sessionId targetId & withServerError

      html <- withRunInIO $ \unlift -> do
        traceM "[Change Target] 1"
        unlift (index sessionId) `catch` \(_ :: SomeException) -> unlift do
          restore
          throwError (err500 { errBody = [i|Invalid target|]})

      traceM "[Change Target] 2"
      pure $ addHeader TargetChanged html


  , themeCss = do
      theme <- Env.getTheme
      dir <- Env.getDataDir
      readFile $
        case theme of
          Dark -> dir </> "dark.css"
          Light -> dir </> "light.css"


  , healthz = pure "ok"
  }



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
