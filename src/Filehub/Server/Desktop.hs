{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Desktop where

import Control.Monad (forM)
import Filehub.ClientPath qualified as ClientPath
import Filehub.Env qualified as Env
import Filehub.Target qualified as Target
import Filehub.Target.Types (Target(..))
import Filehub.Target.Types.TargetView (TargetView(..))
import Filehub.Error ( withServerError )
import Filehub.Monad ( Filehub )
import Filehub.Session.Types (TargetSessionData(..))
import Filehub.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Storage qualified as Storage
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Internal qualified as Template
import Filehub.Server.Internal (withQueryParam)
import Filehub.Types ( SessionId(..), ClientPath, Selected(..))
import Filehub.Server.Handler (ConfirmDesktopOnly)
import Filehub.Storage (getStorage)
import Filehub.Target.Class (IsTarget(..))
import Filehub.ControlPanel qualified as ControlPanel
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)


fileDetailModal :: SessionId -> ConfirmDesktopOnly -> Maybe ClientPath -> Filehub (Html ())
fileDetailModal sessionId _ mClientPath = withServerError do
  clientPath <- withQueryParam mClientPath
  storage <- getStorage sessionId
  root <- Env.getRoot sessionId
  file <- storage.get (ClientPath.fromClientPath root clientPath)
  pure (Template.Desktop.fileDetailModal file)


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath = withServerError do
  clientPath <- withQueryParam mClientPath
  storage <- getStorage sessionId
  root <- Env.getRoot sessionId
  let p = ClientPath.fromClientPath root clientPath
  content <- do
    f <- storage.get p
    storage.read f
  let filename = takeFileName p
  readOnly <- Env.getReadOnly
  pure $ Template.Desktop.editorModal readOnly filename content


contextMenu :: SessionId -> ConfirmDesktopOnly -> [ClientPath] -> Filehub (Html ())
contextMenu sessionId _ clientPaths = withServerError do
  storage <- getStorage sessionId
  root <- Env.getRoot sessionId
  files <- traverse storage.get $ ClientPath.fromClientPath root <$> clientPaths
  readOnly <- Env.getReadOnly
  pure $ Template.Desktop.contextMenu readOnly root files


index :: SessionId -> Filehub (Html ())
index sessionId = do
  readOnly <- Env.getReadOnly
  sideBar' <- sideBar sessionId
  view' <- view sessionId
  layout <- Env.getLayout sessionId & withServerError
  theme <- Env.getSessionTheme sessionId & withServerError
  state <- ControlPanel.getControlPanelState sessionId & withServerError
  pure $ Template.Desktop.index readOnly sideBar' view' layout theme state


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  targets <- Env.getTargets

  targets' <- forM targets $ \(Target backend) -> withServerError do
    let targetId = getTargetIdFromBackend backend
    Target.withTarget sessionId targetId $ \(TargetView target targetData _) -> do
      case targetData.selected of
        Selected _ sels -> pure (target, length sels + 1)
        NoSelection -> pure (target, 0)
  currentTargetView <- Target.currentTarget sessionId & withServerError

  pure $ Template.Desktop.sideBar targets' currentTargetView


view :: SessionId -> Filehub (Html ())
view sessionId = do
  table <- withServerError do
    storage <- getStorage sessionId
    root <- Env.getRoot sessionId
    order <- Env.getSortFileBy sessionId
    layout <- Env.getLayout sessionId & withServerError
    files <- sortFiles order <$> storage.lsCwd
    TargetView target _ _ <- Env.currentTarget sessionId
    selected <- Selected.getSelected sessionId
    pure $ Template.Desktop.table target root files selected order layout
  Template.Desktop.view table <$> pathBreadcrumb sessionId


pathBreadcrumb :: SessionId -> Filehub (Html ())
pathBreadcrumb sessionId =
  Template.pathBreadcrumb
  <$> (Env.getCurrentDir sessionId)
  <*> (Env.getRoot sessionId)
  & withServerError
