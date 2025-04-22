module Filehub.Selected where

import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Data.List (union)
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Domain.Types (ClientPath)
import Filehub.Types (Env, SessionId, Session(..), Selected(..))
import Filehub.Error (FilehubError)
import Filehub.Env qualified as Env
import Filehub.Env.Target qualified as Target


getSelected :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Eff es Selected
getSelected sessionId = (^. #sessionData . #selected) <$> Target.currentTarget sessionId


setSelected :: (Reader Env :> es, IOE :> es) => SessionId -> Selected -> Eff es ()
setSelected sessionId selected = Env.updateSession sessionId $ \s -> s & #targets . ix s.index . #selected .~ selected


toList :: Selected -> [ClientPath]
toList NoSelection = mempty
toList (Selected x xs) = x:xs


fromList :: [ClientPath] -> Selected
fromList (x:xs) = Selected x xs
fromList [] = NoSelection


newtype AsSet = AsSet Selected


instance Semigroup AsSet where
  (AsSet a) <> (AsSet b) = AsSet (fromList (toList a `union` toList b))


instance Monoid AsSet where
  mempty = AsSet NoSelection
