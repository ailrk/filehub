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
  , Storage(..)
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
  , setSessionSharedLinkPermit
  , getSessionSharedLinkPermit
  , getStorage
  , changeCurrentTarget
  , currentTarget
  , withTarget
  , getSessionNotifications
  , getPendingTasks
  , notify
  )
  where

import Control.Applicative (asum)
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String.Interpolate (i)
import Data.Typeable (cast)
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logAttention, logTrace_, logAttention_)
import Effectful.Reader.Dynamic (asks)
import Filehub.Auth.Types (AuthId)
import Filehub.Display qualified as Display
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Locale (Locale)
import Filehub.Monad (Filehub)
import Filehub.Notification.Types (Notification)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Types (TargetView(..))
import Filehub.Storage.File qualified as File
import Filehub.Storage.S3 qualified as S3
import Filehub.Storage.Types (Storage(..))
import Filehub.Types
import Filehub.UserAgent qualified as UserAgent
import Lens.Micro
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude hiding (readFile)
import Target.File (Backend(..), FileSys)
import Target.S3 (S3)
import Target.Types (TargetId, Target (..), getTargetId, handleTarget, targetHandler)
import UnliftIO.STM (TBQueue, TVar, atomically, writeTBQueue)
import Worker.Task (TaskId)
import {-# SOURCE #-} Filehub.Session.Copy qualified as Copy
import {-# SOURCE #-} Filehub.Session.Selected qualified as Selected
import Filehub.SharedLink (SharedLinkPermitSet)
import Data.Map.Strict qualified as Map
import Effectful.Concurrent.STM (readTVarIO)


-- | Get the current target root. The meaning of the root depends on the target. e.g for
-- a normal file system root is the file path, meanwhile S3 has no root, it will always be ""
getRoot :: SessionId -> Filehub FilePath
getRoot sessionId = do
  TargetView (Target t) _ <- currentTarget sessionId
  pure
    . fromMaybe ""
    . asum
    $ [ cast t <&> \(x :: Backend FileSys) -> x.root
      , cast t <&> \(_ :: Backend S3) -> ""
      ]

-- | Get the current working directory of the session.
getCurrentDir :: SessionId -> Filehub FilePath
getCurrentDir sessionId = (^. #sessionData . #currentDir) <$> currentTarget sessionId


-- | Set the current working directory of the session.
setCurrentDir :: SessionId -> FilePath -> Filehub ()
setCurrentDir sessionId path = do
  Session.Pool.update sessionId \s -> s & #targets . ix s.currentTargetId . #currentDir .~ path


-- | Get the file sorting order of the current session.
getSortFileBy :: SessionId -> Filehub SortFileBy
getSortFileBy sessionId = (^. #sessionData . #sortedFileBy) <$> currentTarget sessionId


-- | Set the file sorting order of the current session.
setSortFileBy :: SessionId -> SortFileBy -> Filehub ()
setSortFileBy sessionId order = do
  Session.Pool.update sessionId \s -> s & #targets . ix s.currentTargetId . #sortedFileBy .~ order


-- | Get the session `AuthId`.
getAuthId :: SessionId -> Filehub (Maybe AuthId)
getAuthId sessionId = (^. #authId) <$> Session.Pool.get sessionId


-- | Set the session `AuthId`.
setAuthId :: SessionId -> Maybe AuthId -> Filehub ()
setAuthId sessionId mAuthId = do
  Session.Pool.update sessionId \s -> s & #authId .~ mAuthId


-- | Get the current session layout.
getLayout :: SessionId -> Filehub Layout
getLayout sessionId = (^. #layout) <$> Session.Pool.get sessionId


-- | Set the current session layout.
setLayout :: SessionId -> Layout -> Filehub ()
setLayout sessionId layout = do
  Session.Pool.update sessionId \s -> s & #layout .~ layout


-- | Get the current session theme.
getSessionTheme :: SessionId -> Filehub Theme
getSessionTheme sessionId = (^. #theme) <$> Session.Pool.get sessionId


-- | Set the current session theme.
setSessionTheme :: SessionId -> Theme -> Filehub ()
setSessionTheme sessionId theme = do
  Session.Pool.update sessionId \s -> s & #theme .~ theme


-- | Get the current session theme.
getSessionLocale :: SessionId -> Filehub Locale
getSessionLocale sessionId = (^. #locale) <$> Session.Pool.get sessionId


-- | Set the current session theme.
setSessionLocale :: SessionId -> Locale -> Filehub ()
setSessionLocale sessionId locale = do
  Session.Pool.update sessionId \s -> s & #locale .~ locale


-- | Get the current session display. The display is calculated base on the client screen resolution.
getDisplay :: SessionId -> Filehub Display
getDisplay sessionId = do
  session <- Session.Pool.get sessionId
  case session ^. #resolution of
    Just resolution ->
      case session ^. #deviceType of
        UserAgent.Desktop -> pure Desktop
        UserAgent.Mobile  -> pure $ Display.classify resolution
        UserAgent.Tablet  -> pure $ Display.classify resolution
        UserAgent.Bot     -> pure $ Display.classify resolution
        UserAgent.Unknown -> pure $ Display.classify resolution
    Nothing -> pure NoDisplay


getControlPanelState :: SessionId -> Filehub ControlPanelState
getControlPanelState sessionId = do
  isAnySelected <- Selected.anySelected sessionId
  copyState     <- Copy.getCopyState sessionId
  case (isAnySelected, copyState) of
    (_, Paste {})           -> pure ControlPanelCopied
    (True, CopySelected {}) -> pure ControlPanelSelecting
    (True, NoCopyPaste)     -> pure ControlPanelSelecting
    _                       -> pure ControlPanelDefault


-- | Set the current session theme.
getSessionSharedLinkPermit:: SessionId -> Filehub (Maybe SharedLinkPermitSet)
getSessionSharedLinkPermit sessionId = (^. #sharedLinkPermit) <$> Session.Pool.get sessionId


setSessionSharedLinkPermit :: SessionId -> Maybe SharedLinkPermitSet -> Filehub ()
setSessionSharedLinkPermit sessionId sharedLinkPermit = do
  Session.Pool.update sessionId \s -> s & #sharedLinkPermit .~ sharedLinkPermit


------------------------------
-- Target
------------------------------


currentTarget :: SessionId -> Filehub TargetView
currentTarget sessionId = do
  mSession <- Session.Pool.get sessionId
  targets <- asks @Env (.targets) >>= readTVarIO
  maybe (throwError (FilehubError InvalidSession "Invalid session")) pure do
    targetId          <- mSession ^? #currentTargetId
    targetSessionData <- mSession ^? #targets >>= Map.lookup targetId
    target            <- lookup targetId targets
    pure $ TargetView target targetSessionData


changeCurrentTarget :: SessionId -> TargetId -> Filehub ()
changeCurrentTarget sessionId targetId = do
  logTrace_ [i|Changing target to #{targetId}|]
  TargetView target _ <- currentTarget sessionId
  targets             <- asks @Env (.targets) >>= readTVarIO
  if getTargetId target == targetId
     then pure ()
     else do
       case lookup targetId targets of
         Just _ -> do
           Session.Pool.update sessionId \s -> s & #currentTargetId .~ targetId
         Nothing -> do
           logAttention "Can't find target" (show targetId)
           throwError (FilehubError InvalidSession "Invalid session")


withTarget :: SessionId -> TargetId -> (TargetView -> Storage Filehub -> Filehub a) -> Filehub a
withTarget sessionId targetId action = do
  TargetView saved _ <- currentTarget sessionId
  changeCurrentTarget sessionId targetId
  storage <- getStorage sessionId
  result <- currentTarget sessionId >>= flip action storage
  changeCurrentTarget sessionId (getTargetId saved)
  pure result


------------------------------
-- Storage
------------------------------


getStorage :: SessionId -> Filehub (Storage Filehub)
getStorage sessionId = do
  TargetView target _ <- currentTarget sessionId
  maybe onError pure $ handleTarget target
    [ targetHandler @FileSys     \_ -> fileStorage
    , targetHandler @S3          \_ -> s3Storage
    ]
  where
    s3Storage   = S3.storage sessionId
    fileStorage = File.storage sessionId
    onError     = do
      logAttention_ "[getStorage] target error"
      throwError (FilehubError TargetError "Invalid target")


------------------------------
-- Notification
-----------------------------


getSessionNotifications :: SessionId -> Filehub (TBQueue Notification)
getSessionNotifications sessionId = (^. #notifications) <$> Session.Pool.get sessionId


getPendingTasks :: SessionId -> Filehub (TVar (Set TaskId))
getPendingTasks sessionId = (^. #pendingTasks) <$> Session.Pool.get sessionId


notify :: SessionId -> Notification -> Filehub ()
notify sessionId notification = do
  notifications <- getSessionNotifications sessionId
  atomically $ writeTBQueue notifications notification
