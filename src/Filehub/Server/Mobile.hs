{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Mobile where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Foldable (forM_)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.String.Interpolate (i)
import Effectful ( Eff, (:>), IOE, withRunInIO )
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import Effectful.Reader.Dynamic (Reader)
import Filehub.ClientPath qualified as ClientPath
import Filehub.Env (Env (..), TargetView (..))
import Filehub.Env qualified as Env
import Filehub.Target qualified as Target
import Filehub.Error ( withServerError, FilehubError(..), FilehubError(..), withServerError )
import Filehub.Monad ( Filehub )
import Filehub.Routes (Api (..))
import Filehub.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Storage (Storage)
import Filehub.Storage qualified as Storage
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Types
    ( ClientPath(..),
      NewFile(..),
      NewFolder(..),
      SortFileBy(..),
      UpdatedFile(..),
      Theme(..),
      SessionId(..),
      ClientPath(..),
      NewFile(..),
      NewFolder(..),
      SortFileBy(..),
      UpdatedFile(..),
      Theme(..),
      SessionId(..),
      FilehubEvent (..),
      Selected (..))
import Filehub.Viewer qualified as Viewer
import Filehub.ControlPanel qualified as ControlPanel
import Filehub.Copy qualified as Copy
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Prelude hiding (readFile)
import Servant ( ServerError(..), addHeader, ServerError, err500 )
import Servant.Server (err400)
import Servant.Server.Generic (AsServerT)
import System.FilePath ((</>), takeFileName)
import Text.Printf (printf)
import Log (logAttention_)
import UnliftIO (catch, SomeException)


-- | Server definition
server :: Api (AsServerT Filehub)
server = Api
  { index = fmap Template.Mobile.withDefault . index'


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


  , newFileModal = \_ _ -> pure Template.Mobile.newFileModal


  , newFolderModal = \_ _ -> pure Template.Mobile.newFolderModal


  , fileDetailModal = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        file <- runStorage sessionId $ Storage.get (ClientPath.fromClientPath root clientPath)
        pure (Template.Mobile.fileDetailModal file)


  , editorModal = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        let p = ClientPath.fromClientPath root clientPath
        content <- runStorage sessionId do
          f <- Storage.get p
          Storage.read f
        let filename = takeFileName p
        readOnly <- Env.getReadOnly
        pure $ Template.Mobile.editorModal readOnly filename content


  , search = \sessionId searchWord -> do
      withServerError . runStorage sessionId $ do
        TargetView target _ _ <- Env.currentTarget sessionId & withServerError
        root <- Env.getRoot sessionId
        files <- Storage.lsCwd
        order <- Env.getSortFileBy sessionId
        selected <- Selected.getSelected sessionId
        pure $ Template.Mobile.search searchWord target root files selected order


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
          Template.Mobile.controlPanel
            <$> Env.getReadOnly
            <*> ControlPanel.getControlPanelState sessionId & withServerError


  , upload = \sessionId _ multipart -> do
      runStorage sessionId $ Storage.upload multipart
      index sessionId


  , download = \sessionId mClientPath -> do
      clientPath@(ClientPath path) <- withQueryParam mClientPath
      bs <- runStorage sessionId $ Storage.download clientPath
      pure $ addHeader (printf "attachement; filename=%s" (takeFileName path)) bs


  , copy = \sessionId _ ->
      withServerError do
        Copy.select sessionId
        Copy.copy sessionId
        Template.Mobile.controlPanel
          <$> Env.getReadOnly
          <*> ControlPanel.getControlPanelState sessionId


  , paste = \sessionId _ -> do
      withRunInIO $ \unlift -> do
        unlift (Copy.paste sessionId & withServerError) `catch` \(_ :: SomeException) -> unlift do
          throwError (err500 { errBody = [i|Paste failed|]})
      index sessionId


  , cancel = \sessionId -> do
      clear sessionId
      index sessionId


  , contextMenu = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        let filePath = ClientPath.fromClientPath root clientPath
        file <- runStorage sessionId $ Storage.get filePath
        readOnly <- Env.getReadOnly
        pure $ Template.Mobile.contextMenu readOnly root file


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


  , healthz = pure "ok"
  }


withQueryParam :: (Error ServerError :> es) => Maybe a -> Eff es a
withQueryParam m =
  case m of
    Just a -> pure a
    Nothing -> throwError err400


runStorage :: _ => SessionId -> Eff (Storage : Error FilehubError : es) a -> Eff es a
runStorage sessionId = withServerError . Storage.runStorage sessionId


-- | Hard reset all session data. This inclues the current selection, copy-paste status, etc.
index' :: SessionId -> Filehub (Html ())
index' sessionId = do
  clear sessionId
  index sessionId


index :: SessionId -> Filehub (Html ())
index sessionId =
  Template.Mobile.index
  <$> Env.getReadOnly
  <*> sideBar sessionId
  <*> view sessionId
  <*> (ControlPanel.getControlPanelState sessionId & withServerError)


clear :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clear sessionId = do
  Selected.clearSelectedAllTargets sessionId
  Copy.clearCopyState sessionId


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = withServerError $
  Template.Mobile.sideBar
  <$> Env.getTargets
  <*> Target.currentTarget sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  root <- Env.getRoot sessionId & withServerError
  order <- Env.getSortFileBy sessionId & withServerError
  files <- sortFiles order <$> runStorage sessionId Storage.lsCwd & withServerError
  TargetView target _ _ <- Env.currentTarget sessionId & withServerError
  selected <- Selected.getSelected sessionId & withServerError
  let table = Template.Mobile.table target root files selected order
  Template.Mobile.view table <$> pathBreadcrumb sessionId


pathBreadcrumb :: SessionId -> Filehub (Html ())
pathBreadcrumb sessionId =
  Template.Mobile.pathBreadcrumb
  <$> (Env.getCurrentDir sessionId & withServerError)
  <*> (Env.getRoot sessionId & withServerError)
