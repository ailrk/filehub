module Filehub.Server.Components
  ( index
  , view
  , controlPanel
  , sideBar
  , toolBar
  )
  where

import Data.Function ((&))
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Server.Internal (makeTemplateContext)
import Filehub.Server.Platform.Desktop qualified as Server.Desktop
import Filehub.Server.Platform.Mobile qualified as Server.Mobile
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Filehub.Template qualified as Template
import Filehub.Template.Internal (runTemplate)
import Filehub.Template.Platform.Desktop qualified as Template.Desktop
import Filehub.Template.Platform.Mobile qualified as Template.Mobile
import Filehub.Types (Display (..))
import Lucid ( Html )
import Prelude hiding (init, readFile)


index :: SessionId -> Filehub (Html ())
index sessionId = do
  display <- Session.getDisplay sessionId
  case display of
    NoDisplay -> pure Template.bootstrap
    Desktop   -> Server.Desktop.index sessionId
    Mobile    -> Server.Mobile.index sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  display <- Session.getDisplay sessionId
  case display of
    Desktop   -> Server.Desktop.view sessionId
    Mobile    -> Server.Mobile.view sessionId
    NoDisplay -> Server.Mobile.view sessionId


controlPanel :: SessionId -> Filehub (Html ())
controlPanel sessionId = do
  ctx <- makeTemplateContext sessionId
  display <- Session.getDisplay sessionId
  pure $
    case display of
      Desktop -> runTemplate ctx Template.Desktop.controlPanel
      Mobile  -> runTemplate ctx Template.Mobile.controlPanel
      _       -> runTemplate ctx Template.Mobile.controlPanel


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  display <- Session.getDisplay sessionId
  case display of
    Desktop -> Server.Desktop.sideBar sessionId
    _       -> Server.Mobile.sideBar sessionId


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  display <- Session.getDisplay sessionId
  case display of
    Desktop -> Server.Desktop.toolBar sessionId
    _       -> Server.Mobile.toolBar sessionId
