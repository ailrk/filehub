module Filehub.Selected
  ( getSelected
  , setSelected
  , anySelected
  , countSelected
  , countSelected'
  , clearSelected
  , clearSelectedAllTargets
  , toList
  , fromList
  , elem
  , allSelecteds
  , AsSet(..)
  )
  where


import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Data.List (union)
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Log (Log)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader, asks)
import Filehub.Types ( ClientPath, Env(..), SessionId, Session(..), Selected(..), Target)
import Filehub.Error (FilehubError)
import Filehub.Session qualified as Session
import Filehub.Target qualified as Target
import Prelude hiding (elem)
import Prelude qualified
import Control.Monad (join)


getSelected :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es Selected
getSelected sessionId = (^. #sessionData . #selected) <$> Target.currentTarget sessionId


setSelected :: (Reader Env :> es, IOE :> es) => SessionId -> Selected -> Eff es ()
setSelected sessionId selected = Session.updateSession sessionId $ \s -> s & #targets . ix s.index . #selected .~ selected


anySelected :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es Bool
anySelected sessionId = anySelected'  <$> Session.getSession sessionId


anySelected' :: Session -> Bool
anySelected' session =
  session
  ^. #targets
  & fmap (^. #selected)
  & any (\case { Selected _ _ -> True; NoSelection -> False })


-- | Get all selected files grouped by targets
allSelecteds :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es [(Target, Selected)]
allSelecteds sessionId = do
  session <- Session.getSession sessionId
  let selecteds = session ^. #targets & fmap (^. #selected)
  targets <- asks @Env (.targets)
  pure (allSelecteds' selecteds targets)


allSelecteds' :: [Selected] -> [Target] -> [(Target, Selected)]
allSelecteds' selecteds targets = targets `zip` selecteds


countSelected :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es Int
countSelected sessionId = do
  session <- Session.getSession sessionId
  let selecteds = session ^. #targets & fmap (^. #selected)
  targets <- asks @Env (.targets)
  pure $ countSelected' selecteds targets


countSelected' :: [Selected] -> [Target] -> Int
countSelected' selecteds targets =
  length . join . fmap (toList . snd) $ allSelecteds' selecteds targets


clearSelected :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clearSelected sessionId = setSelected sessionId NoSelection


clearSelectedAllTargets :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clearSelectedAllTargets sessionId = do
  let update sessionData = sessionData & #selected .~ NoSelection
  Session.updateSession sessionId $ \s -> s &  #targets . mapped %~ update


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
