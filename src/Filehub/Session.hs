-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements interfaces for `Session`. Each `Session` corresponds
-- to a browser session, every browser session stores their own session id in cookies
-- in order to identify their `Session`. The backend handlers uses session id to
-- figure out which session we are talking to.
--
-- There should be a one to one matching betwen browser sessions and `Session` in the
-- session pool. A request from one browser session should not be able to change the
-- `Session` of another session.
--
-- === Session pool
-- All `Session` in the system are stored in  `Pool`, when creating, getting, updating
-- sessions, we are querying the session pool with session id. The pool is implemented
-- as a newtype wrapper over Map. There is also a gc thread periodically search for and
-- reclaim expired sessions. This makes sure a long running instance will not accumulate
-- sessions infinitely.
--
-- === Browser state
-- `Session` mantains the current browser state, including the theme the user
-- chose, the sort order, selected files, etc. It manages more than usual web app
-- because filehub is a hypertext based, the server takes more responsibilities.
--
-- === Targets
-- Filehub supports multiple targets. A browser session can focus on one target at a
-- time. Target specific session data are stored in a list so the state is preserved
-- when the user navigate different targets.
--
-- === Authentication
-- `Session` itself has no concept of user identity, it handles user login through the
-- `authId` field. if a user logged in to the system, the authId field will be set, and
-- the user information can be found by lookuping up `ActiveUsers`.
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
  , getSessionLocale
  , setSessionLocale
  , getDisplay
  , getControlPanelState
  , getStorage
  , changeCurrentTarget
  , currentTarget
  , withTarget
  )
  where

import Control.Applicative (asum)
import Data.Generics.Labels ()
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Typeable (cast)
import Effectful (Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Log (Log, logAttention, logTrace_)
import Effectful.Reader.Dynamic (Reader, asks)
import Filehub.Auth.Types (AuthId)
import Filehub.Display qualified as Display
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Locale (Locale)
import {-# SOURCE #-} Filehub.Storage (getStorage)
import Filehub.Session.Pool qualified as Session.Pool
import {-# SOURCE #-} Filehub.Session.Selected qualified as Selected
import {-# SOURCE #-} Filehub.Session.Copy qualified as Copy
import Filehub.Target (getTargetId)
import Filehub.Target.File (Backend(..), FileSys)
import Filehub.Target.S3 (S3)
import Filehub.Target.Types.TargetView (TargetView(..))
import Filehub.Types
import Filehub.UserAgent qualified as UserAgent
import Lens.Micro
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude hiding (readFile)



-- | Get the current target root. The meaning of the root depends on the target. e.g for
-- a normal file system root is the file path, meanwhile S3 has no root, it will always be ""
getRoot :: (Reader Env  :> es,  Error FilehubError :> es,  IOE :> es, Log :> es) => SessionId -> Eff es FilePath
getRoot sessionId = do
  TargetView (Target t) _ _ <- currentTarget sessionId
  pure
    . fromMaybe ""
    . asum
    $ [ cast t <&> \(x :: Backend FileSys) -> x.root
      , cast t <&> \(_ :: Backend S3) -> ""
      ]

-- | Get the current working directory of the session.
getCurrentDir :: (Reader Env  :> es,  Error FilehubError :> es,  IOE :> es, Log :> es) => SessionId -> Eff es FilePath
getCurrentDir sessionId = (^. #sessionData . #currentDir) <$> currentTarget sessionId


-- | Set the current working directory of the session.
setCurrentDir :: (Reader Env  :> es,  IOE :> es) => SessionId -> FilePath -> Eff es ()
setCurrentDir sessionId path = do
  Session.Pool.update sessionId \s -> s & #targets . ix s.index . #currentDir .~ path


-- | Get the file sorting order of the current session.
getSortFileBy :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es SortFileBy
getSortFileBy sessionId = (^. #sessionData . #sortedFileBy) <$> currentTarget sessionId


-- | Set the file sorting order of the current session.
setSortFileBy :: (Reader Env :> es, IOE :> es) => SessionId -> SortFileBy -> Eff es ()
setSortFileBy sessionId order = do
  Session.Pool.update sessionId \s -> s & #targets . ix s.index . #sortedFileBy .~ order


-- | Get the session `AuthId`.
getAuthId :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es (Maybe AuthId)
getAuthId sessionId = (^. #authId) <$> Session.Pool.get sessionId


-- | Set the session `AuthId`.
setAuthId :: (Reader Env :> es, IOE :> es) => SessionId -> Maybe AuthId -> Eff es ()
setAuthId sessionId mAuthId = do
  Session.Pool.update sessionId \s -> s & #authId .~ mAuthId


-- | Get the current session layout.
getLayout :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es Layout
getLayout sessionId = (^. #layout) <$> Session.Pool.get sessionId


-- | Set the current session layout.
setLayout :: (Reader Env :> es, IOE :> es) => SessionId -> Layout -> Eff es ()
setLayout sessionId layout = do
  Session.Pool.update sessionId \s -> s & #layout .~ layout


-- | Get the current session theme.
getSessionTheme :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es Theme
getSessionTheme sessionId = (^. #theme) <$> Session.Pool.get sessionId


-- | Set the current session theme.
setSessionTheme :: (Reader Env :> es, IOE :> es) => SessionId -> Theme -> Eff es ()
setSessionTheme sessionId theme = do
  Session.Pool.update sessionId \s -> s & #theme .~ theme


-- | Get the current session theme.
getSessionLocale :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es Locale
getSessionLocale sessionId = (^. #locale) <$> Session.Pool.get sessionId


-- | Set the current session theme.
setSessionLocale :: (Reader Env :> es, IOE :> es) => SessionId -> Locale -> Eff es ()
setSessionLocale sessionId locale = do
  Session.Pool.update sessionId \s -> s & #locale .~ locale


-- | Get the current session display. The display is calculated base on the client screen resolution.
getDisplay :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es Display
getDisplay sessionId = do
  session <- Session.Pool.get sessionId
  case session ^. #resolution of
    Just resolution ->
      case session ^. #deviceType of
        UserAgent.Desktop -> pure Desktop
        _                 -> pure $ Display.classify resolution
    Nothing -> pure NoDisplay



getControlPanelState :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es ControlPanelState
getControlPanelState sessionId = do
  isAnySelected <- Selected.anySelected sessionId
  copyState     <- Copy.getCopyState sessionId
  case (isAnySelected, copyState) of
    (_, Paste {})           -> pure ControlPanelCopied
    (True, CopySelected {}) -> pure ControlPanelSelecting
    (True, NoCopyPaste)     -> pure ControlPanelSelecting
    _                       -> pure ControlPanelDefault


------------------------------
-- Target
------------------------------

currentTarget :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es TargetView
currentTarget sessionId = do
  mSession <- Session.Pool.get sessionId
  targets <- asks @Env (.targets)
  maybe (throwError (FilehubError InvalidSession "Invalid session")) pure do
    index             <- mSession ^? #index
    targetSessionData <- mSession ^? #targets . ix index
    target            <- targets  ^? ix index
    pure $ TargetView target targetSessionData index


changeCurrentTarget :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> TargetId -> Eff es ()
changeCurrentTarget sessionId targetId = do
  logTrace_ [i|Changing target to #{targetId}|]
  TargetView target _ _ <- currentTarget sessionId
  targets               <- asks @Env (.targets)
  if getTargetId target == targetId
     then pure ()
     else do
       case find (\(_, x) -> getTargetId x == targetId) ([0..] `zip` targets) of
         Just (idx, _) -> do
           Session.Pool.update sessionId \s -> s & #index .~ idx
         Nothing -> do
           logAttention "Can't find target" (show targetId)
           throwError (FilehubError InvalidSession "Invalid session")


withTarget :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> TargetId -> (TargetView -> Eff es a) -> Eff es a
withTarget sessionId targetId action = do
  TargetView saved _ _ <- currentTarget sessionId
  changeCurrentTarget sessionId targetId
  result <- currentTarget sessionId >>= action
  changeCurrentTarget sessionId (getTargetId saved)
  pure result
