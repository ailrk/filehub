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
import Filehub.Storage qualified as Storage
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Server.Internal (runStorage, clear)
import Filehub.ControlPanel qualified as ControlPanel
import Lens.Micro
import Lens.Micro.Platform ()
import Prelude hiding (readFile)


index :: SessionId -> Filehub (Html ())
index sessionId = do
  display <- Env.getDisplay sessionId & withServerError
  clear sessionId
  fmap (Template.withDefault display) $ index' sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  root <- Env.getRoot sessionId & withServerError
  order <- Env.getSortFileBy sessionId & withServerError
  files <- sortFiles order <$> runStorage sessionId Storage.lsCwd & withServerError
  TargetView target _ _ <- Env.currentTarget sessionId & withServerError
  selected <- Selected.getSelected sessionId & withServerError
  let table = Template.Desktop.table target root files selected order
  pure $ Template.Mobile.view table


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
