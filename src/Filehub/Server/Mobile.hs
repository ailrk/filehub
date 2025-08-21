{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Mobile where

import Filehub.Monad ( Filehub )
import Filehub.Types
    ( SessionId(..),
      SessionId(..), ClientPath)
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Template.Internal qualified as Template
import Filehub.Env qualified as Env
import Filehub.Target qualified as Target
import Filehub.Target.Types.TargetView (TargetView(..))
import Filehub.Error ( withServerError, withServerError )
import Filehub.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Server.Internal (withQueryParam)
import Filehub.ControlPanel qualified as ControlPanel
import Lens.Micro
import Lens.Micro.Platform ()
import Prelude hiding (readFile)
import Filehub.Storage (getStorage, Storage(..))
import System.FilePath (takeFileName)
import Filehub.ClientPath qualified as ClientPath


index :: SessionId -> Filehub (Html ())
index sessionId = do
  readOnly <- Env.getReadOnly
  noLogin <- Env.getNoLogin
  sideBar' <- sideBar sessionId
  view' <- view sessionId
  theme <- Env.getSessionTheme sessionId & withServerError
  state <- ControlPanel.getControlPanelState sessionId & withServerError
  selectedCount <- Selected.countSelected sessionId & withServerError
  pure $ Template.Mobile.index readOnly noLogin sideBar' view' theme state selectedCount


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = withServerError $
  Template.Mobile.sideBar
  <$> Env.getTargets
  <*> Target.currentTarget sessionId


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
  pure $ Template.Mobile.editorModal readOnly filename content


view :: SessionId -> Filehub (Html ())
view sessionId = do
  (table, toolBar) <- withServerError do
    storage <- getStorage sessionId
    root <- Env.getRoot sessionId
    order <- Env.getSortFileBy sessionId
    files <- sortFiles order <$> storage.lsCwd
    TargetView target _ _ <- Env.currentTarget sessionId
    selected <- Selected.getSelected sessionId
    let table = Template.Mobile.table target root files selected
    let toolBar = Template.Mobile.sortTool order
    pure (table, toolBar)
  pathBreadcrumb <- Template.pathBreadcrumb
    <$> (Env.getCurrentDir sessionId & withServerError)
    <*> (Env.getRoot sessionId & withServerError)
  pure $ Template.Mobile.view table toolBar pathBreadcrumb
