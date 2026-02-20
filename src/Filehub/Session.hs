{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
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
  , setCurrentDir
  , setSortFileBy
  , setAuthId
  , toggleSidebarCollapsed
  , setLayout
  , setSessionTheme
  , setSessionLocale
  , setSessionTargets
  , setSessionSharedLinkPermit
  , withStorage
  , changeCurrentTarget
  , withTarget
  , attachTarget
  , detachTarget
  , notify
  )
  where

import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Effectful.Concurrent.STM (readTVarIO)
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logAttention, logAttention_, logTrace)
import Effectful.Reader.Dynamic (asks)
import Filehub.Auth.Types (AuthId)
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Locale (Locale)
import Filehub.Monad (Filehub)
import Filehub.Notification.Types (Notification)
import Filehub.Session.Internal (targetToSessionData)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Types (TargetView(..))
import Filehub.SharedLink (SharedLinkPermitSet)
import {-# SOURCE #-} Filehub.Storage.File qualified as File
import Filehub.Storage.S3 qualified as S3
import Filehub.Types
import Lens.Micro
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude hiding (readFile)
import Target.File (FileSys)
import Target.S3 (S3)
import Target.Storage (Storage(..))
import Target.Types (TargetId, AnyTarget (..), getTargetId, handleTarget, targetHandler)
import UnliftIO.STM (TBQueue, atomically, writeTBQueue)
import Data.ClientPath (AbsPath (..))
import Filehub.Session.Access (SessionView(..), viewSession)



-- | Set the current working directory of the session.
setCurrentDir :: SessionId -> AbsPath -> Filehub ()
setCurrentDir sessionId path = do
  Session.Pool.update sessionId \s -> s & #targets . ix s.currentTargetId . #currentDir .~ path


-- | Set the file sorting order of the current session.
setSortFileBy :: SessionId -> SortFileBy -> Filehub ()
setSortFileBy sessionId order = do
  Session.Pool.update sessionId \s -> s & #targets . ix s.currentTargetId . #sortedFileBy .~ order


-- | Set the session `AuthId`.
setAuthId :: SessionId -> Maybe AuthId -> Filehub ()
setAuthId sessionId mAuthId = do
  Session.Pool.update sessionId \s -> s & #authId .~ mAuthId


toggleSidebarCollapsed :: SessionId -> Filehub ()
toggleSidebarCollapsed sessionId = do
  Session.Pool.update sessionId \s ->
    s & #sidebarCollapsed .~ (not (s ^. #sidebarCollapsed))


-- | Set the current session layout.
setLayout :: SessionId -> Layout -> Filehub ()
setLayout sessionId layout = do
  Session.Pool.update sessionId \s -> s & #layout .~ layout



-- | Set the current session theme.
setSessionTheme :: SessionId -> Theme -> Filehub ()
setSessionTheme sessionId theme = do
  Session.Pool.update sessionId \s -> s & #theme .~ theme


-- | Set the current session theme.
setSessionLocale :: SessionId -> Locale -> Filehub ()
setSessionLocale sessionId locale = do
  Session.Pool.update sessionId \s -> s & #locale .~ locale


setSessionTargets :: SessionId -> Map TargetId TargetSessionData -> Filehub ()
setSessionTargets sessionId targets = do
  Session.Pool.update sessionId \s -> s & #targets .~ targets


setSessionSharedLinkPermit :: SessionId -> Maybe SharedLinkPermitSet -> Filehub ()
setSessionSharedLinkPermit sessionId sharedLinkPermit = do
  Session.Pool.update sessionId \s -> s & #sharedLinkPermit .~ sharedLinkPermit


------------------------------
-- Target
------------------------------


changeCurrentTarget :: SessionId -> TargetId -> Filehub ()
changeCurrentTarget sessionId targetId = do
  SessionView { currentTarget = TargetView target _ } <- viewSession sessionId
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
  SessionView { currentTarget = TargetView saved _ } <- viewSession sessionId
  changeCurrentTarget sessionId targetId
  withStorage sessionId \storage -> do
    SessionView { currentTarget } <- viewSession sessionId
    result <- action currentTarget storage
    changeCurrentTarget sessionId (getTargetId saved)
    pure result


attachTarget :: SessionId -> AnyTarget -> Filehub ()
attachTarget sessionId target = do
  SessionView { currentTarget = TargetView current _ } <- viewSession sessionId
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


withStorage :: SessionId -> (Storage Filehub -> Filehub a) -> Filehub a
withStorage sessionId f = do
  SessionView { currentTarget = TargetView current _ } <- viewSession sessionId
  fromMaybe onError $ handleTarget current
    [ targetHandler @FileSys \_ -> f fileStorage
    , targetHandler @S3      \_ -> f s3Storage
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


notify :: SessionId -> Notification -> Filehub ()
notify sessionId notification = do
  notifications <- getSessionNotifications sessionId
  atomically $ writeTBQueue notifications notification
