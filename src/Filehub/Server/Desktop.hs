{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Desktop where

import Filehub.ClientPath qualified as ClientPath
import Filehub.Env (TargetView (..))
import Filehub.Env qualified as Env
import Filehub.Target qualified as Target
import Filehub.Error ( withServerError )
import Filehub.Monad ( Filehub )
import Filehub.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Storage qualified as Storage
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Internal qualified as Template
import Filehub.Server.Internal (withQueryParam, clear)
import Filehub.Types
    ( SessionId(..),
      SessionId(..), ClientPath)
import Filehub.ControlPanel qualified as ControlPanel
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Filehub.Server.Context.Resolution (ConfirmDesktopOnly)
import Filehub.Storage (getStorage)


index :: SessionId -> Filehub (Html ())
index sessionId = do
  clear sessionId
  index' sessionId


fileDetailModal :: SessionId -> ConfirmDesktopOnly -> Maybe ClientPath -> Filehub (Html ())
fileDetailModal sessionId _ mClientPath = withServerError do
  clientPath <- withQueryParam mClientPath
  storage <- getStorage sessionId
  root <- Env.getRoot sessionId
  file <- storage.get (ClientPath.fromClientPath root clientPath)
  pure (Template.Desktop.fileDetailModal file)


editorModal :: SessionId -> ConfirmDesktopOnly -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId _ mClientPath = withServerError do
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


contextMenu :: SessionId -> ConfirmDesktopOnly -> Maybe ClientPath -> Filehub (Html ())
contextMenu sessionId _ mClientPath = withServerError do
  clientPath <- withQueryParam mClientPath
  storage <- getStorage sessionId
  root <- Env.getRoot sessionId
  let filePath = ClientPath.fromClientPath root clientPath
  file <- storage.get filePath
  readOnly <- Env.getReadOnly
  pure $ Template.Desktop.contextMenu readOnly root file


index' :: SessionId -> Filehub (Html ())
index' sessionId =
  Template.Desktop.index
  <$> Env.getReadOnly
  <*> sideBar sessionId
  <*> view sessionId
  <*> (ControlPanel.getControlPanelState sessionId & withServerError)


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId =
  Template.Desktop.sideBar
  <$> Env.getTargets
  <*> Target.currentTarget sessionId
  & withServerError


view :: SessionId -> Filehub (Html ())
view sessionId = do
  table <- withServerError do
    storage <- getStorage sessionId
    root <- Env.getRoot sessionId
    order <- Env.getSortFileBy sessionId
    files <- sortFiles order <$> storage.lsCwd
    TargetView target _ _ <- Env.currentTarget sessionId
    selected <- Selected.getSelected sessionId
    pure $ Template.Desktop.table target root files selected order
  Template.Desktop.view table <$> pathBreadcrumb sessionId


pathBreadcrumb :: SessionId -> Filehub (Html ())
pathBreadcrumb sessionId =
  Template.pathBreadcrumb
  <$> (Env.getCurrentDir sessionId)
  <*> (Env.getRoot sessionId)
  & withServerError
