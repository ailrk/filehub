module Filehub.Selected
  ( getSelected
  , setSelected
  , toList
  , fromList
  , elem
  , AsSet(..)
  )
  where

import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Data.List (union)
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Types
    ( ClientPath, Env, SessionId, Session(..), Selected(..) )
import Filehub.Error (FilehubError)
import Filehub.Env qualified as Env
import Filehub.Env.Target qualified as Target
import Prelude hiding (elem)
import Prelude qualified


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


elem :: ClientPath -> Selected -> Bool
elem _ NoSelection = False
elem path (Selected x xs) = path == x || path `Prelude.elem` xs


newtype AsSet = AsSet Selected


instance Semigroup AsSet where
  (AsSet a) <> (AsSet b) = AsSet (fromList (toList a `union` toList b))


instance Monoid AsSet where
  mempty = AsSet NoSelection
