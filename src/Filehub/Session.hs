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
  , attachTarget
  , detachTarget
  , withTarget
  , get
  , set
  , SessionGet(..)
  , SessionSet(..)
  )
  where

import Data.Generics.Labels ()
import Filehub.Session.Types (TargetView(..))
import Filehub.Types
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude hiding (readFile)
import Target.Storage (Storage(..))

import Data.ClientPath (AbsPath (..), Root (..))
import Filehub.Auth.Types (AuthId)
import Filehub.Locale (Locale)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.SharedLink (SharedLinkPermitSet)
import Filehub.Notification.Types (Notification)
import Data.Set (Set)
import Worker.Task (TaskId)
import Filehub.Error (FilehubError(FilehubError), Error' (..))
import Data.Map qualified as Map
import Filehub.UserAgent qualified as UserAgent
import Filehub.Display qualified as Display
import Filehub.Session.Internal (targetToSessionData)
import Target.S3 (S3)
import Target.File (Target(..), FileSys)
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Applicative (asum)
import Data.Typeable (cast)
import Data.Functor ((<&>))
import {-# SOURCE #-} Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import {-# SOURCE #-} Filehub.Storage.S3 qualified as S3
import {-# SOURCE #-} Filehub.Storage.File qualified as File
import Target.Types (handleTarget, targetHandler, AnyTarget (..), HasTargetId (..), TargetId)
import UnliftIO (finally, throwIO)
import {-# SOURCE #-} Filehub.Auth.OIDC (SomeOIDCFlow)
import Filehub.Monad (Filehub)
import UnliftIO.STM (TBQueue, TVar)
import Log (logAttention_, logTrace, logAttention)
import Control.Monad.Reader (asks)
import UnliftIO.STM (readTVarIO)


attachTarget :: SessionId -> AnyTarget -> Filehub ()
attachTarget sessionId target = do
  TargetView current _ <- get sessionId (.currentTarget)
  if current == target
     then pure ()
     else do
       Session.Pool.update sessionId \session -> do
         session { targets = Map.insert (getTargetId target) (targetToSessionData target) session.targets
                 }


detachTarget :: HasTargetId t => SessionId -> t -> Filehub ()
detachTarget sessionId target = do
  let tid = getTargetId target
  Session.Pool.update sessionId \session -> do
    session { targets = Map.delete tid session.targets
            }


withTarget :: HasTargetId t => SessionId -> t -> Filehub a -> Filehub a
withTarget sid t action = do
  oldS <- Session.Pool.get sid
  let oldTid = oldS.currentTargetId
  (newSessionSet sid).currentTarget (getTargetId t)
  action `finally` (newSessionSet sid).currentTarget oldTid


get :: SessionId -> (SessionGet -> Filehub a) -> Filehub a
get sessionId field = do
  let viewRecord = newSessionGet sessionId
  field viewRecord


set :: SessionId -> (SessionSet -> val -> Filehub ()) -> val -> Filehub ()
set sessionId field val = do
  let setRecord = newSessionSet sessionId
  field setRecord val


data SessionGet = SessionGet
  { currentDir        :: Filehub AbsPath
  , root              :: Filehub Root
  , display           :: Filehub Display
  , sortedFileBy      :: Filehub SortFileBy
  , selected          :: Filehub Selected
  , authId            :: Filehub (Maybe AuthId)
  , sidebarCollapsed  :: Filehub Bool
  , layout            :: Filehub Layout
  , theme             :: Filehub Theme
  , locale            :: Filehub Locale
  , targetViews       :: Filehub [TargetView]
  , controlPanelState :: Filehub (ControlPanelState)
  , sharedLinkPermit  :: Filehub (Maybe SharedLinkPermitSet)
  , oidcFlow          :: Filehub (Maybe SomeOIDCFlow)
  , notifications     :: Filehub (TBQueue Notification)
  , pendingTasks      :: Filehub (TVar (Set TaskId))
  , storage           :: Filehub (Storage Filehub)
  , currentTarget     :: Filehub TargetView
  }


data SessionSet = SessionSet
  { currentDir        :: AbsPath -> Filehub ()
  , sortedFileBy      :: SortFileBy -> Filehub ()
  , selected          :: Selected -> Filehub  ()
  , authId            :: Maybe AuthId -> Filehub ()
  , sidebarCollapsed  :: Bool -> Filehub ()
  , layout            :: Layout -> Filehub ()
  , theme             :: Theme -> Filehub ()
  , locale            :: Locale -> Filehub ()
  , sharedLinkPermit  :: Maybe SharedLinkPermitSet -> Filehub ()
  , currentTarget     :: TargetId -> Filehub ()
  , oidcFlow          :: Maybe SomeOIDCFlow -> Filehub ()
  , notifications     :: TBQueue Notification -> Filehub ()
  , pendingTasks      :: TVar (Set TaskId) -> Filehub ()
  }


newSessionGet :: SessionId -> SessionGet
newSessionGet sessionId =
  let display = do
        s <- Session.Pool.get sessionId
        case s.resolution of
          Just resolution ->
            case s.deviceType of
              UserAgent.Desktop -> pure $ Desktop
              UserAgent.Mobile  -> pure $ Display.classify resolution
              UserAgent.Tablet  -> pure $ Display.classify resolution
              UserAgent.Bot     -> pure $ Display.classify resolution
              UserAgent.Unknown -> pure $ Display.classify resolution
          Nothing -> pure NoDisplay


      currentDir = do
        (TargetView _ td) <- currentTarget
        pure td.currentDir


      sortedFileBy = do
        (TargetView _ td) <- currentTarget
        pure td.sortedFileBy


      selected = do
        (TargetView _ td) <- currentTarget
        pure td.selected


      root = do
        (TargetView (AnyTarget tgt) _) <- currentTarget
        fromMaybe (pure $ Root (AbsPath "")) . asum $
          [ cast tgt <&> \(x :: Target FileSys) -> pure x.root
          , cast tgt <&> \(_ :: Target S3) -> pure (Root (AbsPath ""))
          ]


      targetViews = do
        s <- Session.Pool.get sessionId
        let targetIds =  Map.keys s.targets
        targets       <- filter ((`elem` targetIds) . fst) <$> (asks (.targets) >>= readTVarIO)
        pure $
          flip mapMaybe targets \(targetId, target) ->
            case Map.lookup targetId s.targets of
              Just targetData -> pure (TargetView target targetData)
              Nothing         -> Nothing


      controlPanelState = do
        isAnySelected <- Selected.anySelected sessionId
        copyState     <- Copy.getCopyState sessionId
        case (isAnySelected, copyState) of
          (_, Paste {})           -> pure ControlPanelCopied
          (True, CopySelected {}) -> pure ControlPanelSelecting
          (True, NoCopyPaste)     -> pure ControlPanelSelecting
          _                       -> pure ControlPanelDefault


      storage = do
        (TargetView t _) <- currentTarget
        let s3Storage   = S3.storage sessionId
            fileStorage = File.storage sessionId
            onError     = do
              logAttention_ "[ssshuu] Target error"
              throwIO (FilehubError TargetError "Invalid target")


        fromMaybe onError $ handleTarget t
          [ targetHandler @FileSys \_ -> pure fileStorage
          , targetHandler @S3      \_ -> pure s3Storage
          ]


      currentTarget :: Filehub TargetView
      currentTarget = do
        s <- Session.Pool.get sessionId
        targets <- asks (.targets) >>= readTVarIO
        maybe (throwIO (FilehubError InvalidSession "Invalid session")) pure do
          let targetId      = s.currentTargetId
          targetSessionData <- Map.lookup targetId s.targets
          target            <- lookup targetId targets
          pure $ TargetView target targetSessionData

      g = Session.Pool.get sessionId

    in SessionGet
      { currentDir        = currentDir
      , root              = root
      , display           = display
      , sortedFileBy        = sortedFileBy
      , selected          = selected
      , authId            = g <&> (.authId)
      , sidebarCollapsed  = g <&> (.sidebarCollapsed)
      , layout            = g <&> (.layout)
      , theme             = g <&> (.theme)
      , locale            = g <&> (.locale)
      , targetViews       = targetViews
      , controlPanelState = controlPanelState
      , sharedLinkPermit  = g <&> (.sharedLinkPermit)
      , currentTarget     = currentTarget
      , oidcFlow          = g <&> (.oidcFlow)
      , notifications     = g <&> (.notifications)
      , pendingTasks      = g <&> (.pendingTasks)
      , storage           = storage
      }


newSessionSet :: SessionId -> SessionSet
newSessionSet sessionId =
  let upS :: (Session -> Session) -> Filehub ()
      upS f = Session.Pool.update sessionId f

      upT :: (TargetSessionData -> TargetSessionData) -> Filehub ()
      upT f = upS $ \s -> s { targets = Map.adjust f s.currentTargetId s.targets }

      currentDir a = upT (\td -> td { currentDir = a })

      sortedFileBy a = upT (\td -> td { sortedFileBy = a })

      selected a = upT (\td -> td { selected = a })

      authId a = upS (\s -> s { authId = a })

      sidebarCollapsed a = upS (\s -> s { sidebarCollapsed = a })

      layout a = upS (\s -> s { layout = a })

      theme a = upS (\s -> s { theme = a })

      locale a = upS (\s -> s { locale = a })

      sharedLinkPermit a = upS (\s -> s { sharedLinkPermit = a })

      notifications a = upS (\s -> s { notifications = a })

      oidcFlow a = upS (\s -> s { oidcFlow = a })

      pendingTasks a = upS (\s -> s { pendingTasks = a })

      currentTarget tid = do
          TargetView target _ <- get sessionId (.currentTarget)
          targets <- asks (.targets) >>= readTVarIO
          if getTargetId target == tid
             then pure ()
             else do
               case lookup tid targets of
                 Just _ -> do
                   logTrace "[vccxxa] Changing target" (show tid)
                   upS (\s -> s { currentTargetId = tid })
                 Nothing -> do
                   logAttention "[vccxxa] Can't change to target" (show tid)
                   throwIO (FilehubError InvalidSession "Invalid session")

   in
    SessionSet
      { currentDir        = currentDir
      , sortedFileBy      = sortedFileBy
      , selected          = selected
      , authId            = authId
      , sidebarCollapsed  = sidebarCollapsed
      , layout            = layout
      , theme             = theme
      , locale            = locale
      , sharedLinkPermit  = sharedLinkPermit
      , notifications     = notifications
      , oidcFlow          = oidcFlow
      , pendingTasks      = pendingTasks
      , currentTarget     = currentTarget
      }

