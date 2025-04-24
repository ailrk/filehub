module Filehub.ControlPanel (getControlPanelState) where

import Lens.Micro.Platform ()
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Types
    ( Env, SessionId, ControlPanelState(..), CopyState (..))
import Filehub.Error (FilehubError)
import Prelude hiding (elem)
import Filehub.Selected qualified as Selected
import Filehub.Copy qualified as Copy


getControlPanelState :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Eff es ControlPanelState
getControlPanelState sessionId = do
  isAnySelected <- Selected.anySelected sessionId
  copyState <- Copy.getCopyState sessionId
  case (isAnySelected, copyState) of
    (_, Paste {}) -> pure ControlPanelCopied
    (True, CopySelected {}) -> pure ControlPanelSelecting
    (True, NoCopyPaste) -> pure ControlPanelSelecting
    _ -> pure ControlPanelDefault
