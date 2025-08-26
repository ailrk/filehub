module Filehub.Session
  ( Session(..)
  , SessionId(..)
  , TargetView(..)
  , getRoot
  , getCurrentDir
  , setCurrentDir
  , getSortFileBy
  , setSortFileBy
  , getAuthId
  , setAuthId
  , getLayout
  , setLayout
  , getSessionTheme
  , setSessionTheme
  , getDisplay
  , newSession
  , getSession
  , updateSession
  , currentTarget
  , changeCurrentTarget
  )
  where

import Lens.Micro
import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Filehub.Types ( Env(..), Session(..), Target(..), Display (..), SessionId(..), Theme, SortFileBy)
import Filehub.Session.Pool (getSession, updateSession, newSession)
import Filehub.Target (currentTarget, changeCurrentTarget)
import Filehub.Target qualified as Target
import Filehub.Display qualified as Display
import Filehub.Target.File (Backend(..), FileSys)
import Filehub.Target.S3 (S3)
import Filehub.UserAgent qualified as UserAgent
import Filehub.Target.Types.TargetView (TargetView(..))
import Control.Applicative (asum)
import Data.Typeable (cast)
import Data.Maybe (fromMaybe)
import Effectful (Eff, (:>), IOE)
import Effectful.Log (Log)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Error (FilehubError)
import Effectful.Error.Dynamic (Error)
import Filehub.Layout (Layout)
import Filehub.Auth.Types (AuthId)



-- | Get the current target root. The meaning of the root depends on the target. e.g for
-- a normal file system root is the file path, meanwhile S3 has no root, it will always be ""
getRoot :: (Reader Env  :> es,  Error FilehubError :> es,  IOE :> es, Log :> es) => SessionId -> Eff es FilePath
getRoot sessionId = do
  TargetView (Target t) _ _ <- currentTarget sessionId
  pure $
    fromMaybe "" . asum $
      [ cast t <&> \(x :: Backend FileSys) -> x.root
      , cast t <&> \(_ :: Backend S3) -> ""
      ]


-- | Get the current working directory of the session.
getCurrentDir :: (Reader Env  :> es,  Error FilehubError :> es,  IOE :> es, Log :> es) => SessionId -> Eff es FilePath
getCurrentDir sessionId = (^. #sessionData . #currentDir) <$> Target.currentTarget sessionId


-- | Set the current working directory of the session.
setCurrentDir :: (Reader Env  :> es,  IOE :> es) => SessionId -> FilePath -> Eff es ()
setCurrentDir sessionId path = do
  updateSession sessionId $ \s -> s & #targets . ix s.index . #currentDir .~ path


-- | Get the file sorting order of the current session.
getSortFileBy :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es SortFileBy
getSortFileBy sessionId = (^. #sessionData . #sortedFileBy) <$> Target.currentTarget sessionId


-- | Set the file sorting order of the current session.
setSortFileBy :: (Reader Env :> es, IOE :> es) => SessionId -> SortFileBy -> Eff es ()
setSortFileBy sessionId order = do
  updateSession sessionId (\s -> s & #targets . ix s.index . #sortedFileBy .~ order)


-- | Get the session `AuthId`.
getAuthId :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es (Maybe AuthId)
getAuthId sessionId = (^. #authId) <$> getSession sessionId


-- | Set the session `AuthId`.
setAuthId :: (Reader Env :> es, IOE :> es) => SessionId -> Maybe AuthId -> Eff es ()
setAuthId sessionId mAuthId = do
  updateSession sessionId (\s -> s & #authId .~ mAuthId)


-- | Get the current session layout.
getLayout :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es Layout
getLayout sessionId = (^. #layout) <$> getSession sessionId


-- | Set the current session layout.
setLayout :: (Reader Env :> es, IOE :> es) => SessionId -> Layout -> Eff es ()
setLayout sessionId layout = do
  updateSession sessionId (\s -> s & #layout .~ layout)


-- | Get the current session theme.
getSessionTheme :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es Theme
getSessionTheme sessionId = (^. #theme) <$> getSession sessionId


-- | Set the current session theme.
setSessionTheme :: (Reader Env :> es, IOE :> es) => SessionId -> Theme -> Eff es ()
setSessionTheme sessionId theme = do
  updateSession sessionId (\s -> s & #theme .~ theme)


-- | Get the current session display. The display is calculated base on the client screen resolution.
getDisplay :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es Display
getDisplay sessionId = do
  session <- getSession sessionId
  case session ^. #resolution of
    Just resolution ->
      case session ^. #deviceType of
        UserAgent.Desktop -> pure Desktop
        _ -> pure $ Display.classify resolution
    Nothing -> pure NoDisplay
