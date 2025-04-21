module Filehub.MultiSelect where

import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Types (Env, SessionId, Session(..), Selected(..))
import Filehub.Error (FilehubError)
import Filehub.Env qualified as Env
import Filehub.Env.Target qualified as Target


getSelected :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Eff es Selected
getSelected sessionId = (^. #sessionData . #selected) <$> Target.currentTarget sessionId


setSelected :: (Reader Env :> es, IOE :> es) => SessionId -> Selected -> Eff es ()
setSelected sessionId selected = Env.updateSession sessionId $ \s -> s & #targets . ix s.index . #selected .~ selected
