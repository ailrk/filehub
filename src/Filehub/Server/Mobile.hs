{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Mobile where

import Filehub.Monad ( Filehub )
import Filehub.Types
    ( SessionId(..),
      SessionId(..))
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Template.Internal qualified as Template
import Filehub.Env (TargetView (..))
import Filehub.Env qualified as Env
import Filehub.Target qualified as Target
import Filehub.Error ( withServerError, withServerError )
import Filehub.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Server.Internal (clear)
import Filehub.ControlPanel qualified as ControlPanel
import Lens.Micro
import Lens.Micro.Platform ()
import Prelude hiding (readFile)
import Filehub.Storage (getStorage, Storage(..))


index :: SessionId -> Filehub (Html ())
index sessionId = do
  clear sessionId
  index' sessionId


index' :: SessionId -> Filehub (Html ())
index' sessionId =
  Template.Mobile.index
  <$> Env.getReadOnly
  <*> sideBar sessionId
  <*> view sessionId
  <*> (ControlPanel.getControlPanelState sessionId & withServerError)


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = withServerError $
  Template.Mobile.sideBar
  <$> Env.getTargets
  <*> Target.currentTarget sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  table <- withServerError do
    storage <- getStorage sessionId
    root <- Env.getRoot sessionId
    order <- Env.getSortFileBy sessionId
    files <- sortFiles order <$> storage.lsCwd
    TargetView target _ _ <- Env.currentTarget sessionId
    selected <- Selected.getSelected sessionId
    pure $ Template.Mobile.table target root files selected order
  pathBreadcrumb <- Template.pathBreadcrumb
    <$> (Env.getCurrentDir sessionId & withServerError)
    <*> (Env.getRoot sessionId & withServerError)
  pure $ Template.Mobile.view table pathBreadcrumb
