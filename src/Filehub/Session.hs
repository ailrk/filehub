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
  , getSidebarCollapsed
  , toggleSidebarCollapsed
  , getLayout
  , setLayout
  , getSessionTheme
  , setSessionTheme
  , getSessionLocale
  , setSessionLocale
  , getSessionTargets
  , setSessionTargets
  , getSessionTargetViews
  , getDisplay
  , getControlPanelState
  , setSessionSharedLinkPermit
  , getSessionSharedLinkPermit
  , getStorage
  , changeCurrentTarget
  , currentTarget
  , withTarget
  , attachTarget
  , detachTarget
  , getSessionNotifications
  , getPendingTasks
  , notify
  )
  where

import Control.Applicative (asum)
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Typeable (cast)
import Effectful.Concurrent.STM (readTVarIO)
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logAttention, logAttention_, logTrace)
import Effectful.Reader.Dynamic (asks)
import Filehub.Auth.Types (AuthId)
import Filehub.Display qualified as Display
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Locale (Locale)
import Filehub.Monad (Filehub)
import Filehub.Notification.Types (Notification)
import Filehub.Session.Internal (targetToSessionData)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Types (TargetView(..))
import Filehub.SharedLink (SharedLinkPermitSet)
import Filehub.Storage.File qualified as File
import Filehub.Storage.S3 qualified as S3
import Filehub.Types
import Filehub.UserAgent qualified as UserAgent
import Lens.Micro
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude hiding (readFile)
import Target.File (TargetBackend(..), FileSys)
import Target.S3 (S3)
import Target.Storage (Storage(..))
import Target.Types (TargetId, Target (..), getTargetId, handleTarget, targetHandler)
import UnliftIO.STM (TBQueue, TVar, atomically, writeTBQueue)
import Worker.Task (TaskId)
import {-# SOURCE #-} Filehub.Session.Copy qualified as Copy
import {-# SOURCE #-} Filehub.Session.Selected qualified as Selected


-- | Get the current target root. The meaning of the root depends on the target. e.g for
-- a normal file system root is the file path, meanwhile S3 has no root, it will always be ""
getRoot :: SessionId -> Filehub FilePath
getRoot sessionId = do
  TargetView (Target t) _ <- currentTarget sessionId
  pure
    . fromMaybe ""
    . asum
    $ [ cast t <&> \(x :: TargetBackend FileSys) -> x.root
      , cast t <&> \(_ :: TargetBackend S3) -> ""
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


getSidebarCollapsed :: SessionId -> Filehub Bool
getSidebarCollapsed sessionId = (^. #sidebarCollapsed) <$> Session.Pool.get sessionId


toggleSidebarCollapsed :: SessionId -> Filehub ()
toggleSidebarCollapsed sessionId = do
  Session.Pool.update sessionId \s ->
    s & #sidebarCollapsed .~ (not (s ^. #sidebarCollapsed))


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


getSessionTargets :: SessionId -> Filehub (Map TargetId TargetSessionData)
getSessionTargets sessionId = (^. #targets) <$> Session.Pool.get sessionId


setSessionTargets :: SessionId -> Map TargetId TargetSessionData -> Filehub ()
setSessionTargets sessionId targets = do
  Session.Pool.update sessionId \s -> s & #targets .~ targets


getSessionTargetViews :: SessionId -> Filehub [TargetView]
getSessionTargetViews sessionId = do
  sessionTargetdata <- getSessionTargets sessionId
  let targetIds     =  Map.keys sessionTargetdata
  targets           <- filter ((`elem` targetIds) . fst) <$> (asks @Env (.targets) >>= readTVarIO)
  let targetViews = flip mapMaybe targets \(targetId, target) ->
        case Map.lookup targetId sessionTargetdata of
          Just targetData -> pure (TargetView target targetData)
          Nothing         -> Nothing
  pure targetViews


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
  TargetView target _ <- currentTarget sessionId
  targets             <- asks @Env (.targets) >>= readTVarIO
  if getTargetId target == targetId
     then pure ()
     else do
       case lookup targetId targets of
         Just _ -> do
           logTrace "[vccxxa] Changing target" (show targetId)
           Session.Pool.update sessionId \s -> s & #currentTargetId .~ targetId
         Nothing -> do
           logAttention "[vccxxa] Can't change to target" (show targetId)
           throwError (FilehubError InvalidSession "Invalid session")


withTarget :: SessionId -> TargetId -> (TargetView -> Storage Filehub -> Filehub a) -> Filehub a
withTarget sessionId targetId action = do
  TargetView saved _ <- currentTarget sessionId
  changeCurrentTarget sessionId targetId
  storage <- getStorage sessionId
  result <- currentTarget sessionId >>= flip action storage
  changeCurrentTarget sessionId (getTargetId saved)
  pure result


attachTarget :: SessionId -> Target -> Filehub ()
attachTarget sessionId target = do
  TargetView current _ <- currentTarget sessionId
  if current == target
     then pure ()
     else do
       Session.Pool.update sessionId \session -> do
         session { targets = Map.insert (getTargetId target) (targetToSessionData target) session.targets
                 }


detachTarget :: SessionId -> TargetId -> Filehub ()
detachTarget sessionId targetId = do
  Session.Pool.update sessionId \session -> do
    session { targets = Map.delete targetId session.targets
            }


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
      logAttention_ "[ssshuu] Target error"
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
