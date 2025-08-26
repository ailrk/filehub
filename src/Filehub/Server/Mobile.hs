{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Mobile where

import Effectful.Reader.Dynamic (ask, asks)
import Filehub.Monad ( Filehub )
import Filehub.Types ( ClientPath)
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Template.Internal qualified as Template
import Filehub.Target qualified as Target
import Filehub.Target.Types.TargetView (TargetView(..))
import Filehub.Error ( withServerError, withServerError )
import Filehub.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Server.Internal (withQueryParam)
import Filehub.ControlPanel qualified as ControlPanel
import Filehub.ClientPath qualified as ClientPath
import Filehub.Env (Env)
import Filehub.Env qualified as Env
import Filehub.Session qualified as Session
import Filehub.Session (SessionId)
import Lens.Micro
import Lens.Micro.Platform ()
import Prelude hiding (readFile)
import Filehub.Storage (getStorage, Storage(..))
import System.FilePath (takeFileName)


index :: SessionId -> Filehub (Html ())
index sessionId = do
  readOnly <- asks @Env (.readOnly)
  noLogin <- Env.hasNoLogin <$> ask @Env
  sideBar' <- sideBar sessionId
  view' <- view sessionId
  theme <- Session.getSessionTheme sessionId & withServerError
  state <- ControlPanel.getControlPanelState sessionId & withServerError
  selectedCount <- Selected.countSelected sessionId & withServerError
  pure $ Template.Mobile.index readOnly noLogin sideBar' view' theme state selectedCount


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = withServerError $
  Template.Mobile.sideBar
  <$> asks @Env (.targets)
  <*> Target.currentTarget sessionId


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath = withServerError do
  clientPath <- withQueryParam mClientPath
  storage <- getStorage sessionId
  root <- Session.getRoot sessionId
  let p = ClientPath.fromClientPath root clientPath
  content <- do
    f <- storage.get p
    storage.read f
  let filename = takeFileName p
  readOnly <- asks @Env (.readOnly)
  pure $ Template.Mobile.editorModal readOnly filename content


view :: SessionId -> Filehub (Html ())
view sessionId = do
  (table, toolBar) <- withServerError do
    storage <- getStorage sessionId
    root <- Session.getRoot sessionId
    order <- Session.getSortFileBy sessionId
    files <- sortFiles order <$> storage.lsCwd
    TargetView target _ _ <- Session.currentTarget sessionId
    selected <- Selected.getSelected sessionId
    let table = Template.Mobile.table target root files selected
    let toolBar = Template.Mobile.sortTool order
    pure (table, toolBar)
  pathBreadcrumb <- Template.pathBreadcrumb
    <$> (Session.getCurrentDir sessionId & withServerError)
    <*> (Session.getRoot sessionId & withServerError)
  pure $ Template.Mobile.view table toolBar pathBreadcrumb
