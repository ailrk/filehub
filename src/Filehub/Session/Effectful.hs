{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Filehub.Session.Effectful where

import Data.ClientPath (AbsPath (..))
import Filehub.Monad (IsFilehub)
import Filehub.Display (Display (..))
import Filehub.Sort (SortFileBy)
import Filehub.Auth.Types (AuthId)
import Filehub.Types (Layout, ControlPanelState (..), Env(..), CopyState (..), Selected)
import Filehub.Theme (Theme (..))
import Filehub.Locale (Locale)
import Filehub.Session.Types (TargetView(..), SessionId, Session(..), TargetSessionData(..))
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.SharedLink (SharedLinkPermitSet)
import Effectful.Concurrent.STM (TBQueue, TVar, readTVarIO)
import Filehub.Notification.Types (Notification)
import Data.Set (Set)
import Worker.Task (TaskId)
import Effectful.Reader.Dynamic (asks)
import Effectful.Error.Dynamic (throwError)
import Effectful.Dispatch.Dynamic
import Filehub.Error (FilehubError(FilehubError), Error' (..))
import Data.Map qualified as Map
import Filehub.UserAgent qualified as UserAgent
import Filehub.Display qualified as Display
import Filehub.Session.Internal (targetToSessionData)
import Target.S3 (S3)
import Target.File (Target(..), FileSys)
import Target.Storage (Storage(..))
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Applicative (asum)
import Data.Typeable (cast)
import Data.Functor ((<&>))
import {-# SOURCE #-} Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import {-# SOURCE #-} Filehub.Storage.S3 qualified as S3
import {-# SOURCE #-} Filehub.Storage.File qualified as File
import Effectful.Log (logAttention_, logTrace, logAttention)
import Target.Types (handleTarget, targetHandler, AnyTarget (..), HasTargetId (..), TargetId)
import Effectful (Effect, DispatchOf, Dispatch(..), Eff, (:>))
import UnliftIO (finally)
import {-# SOURCE #-} Filehub.Auth.OIDC (SomeOIDCFlow)


data SessionEff :: Effect where
  GetSessionGet :: IsFilehub es => SessionEff m (SessionGet es)
  GetSessionSet :: IsFilehub es => SessionEff m (SessionSet es)
  WithTarget    :: TargetId -> m a -> SessionEff m a


type instance DispatchOf SessionEff = Dynamic


runSessionEff :: IsFilehub es => SessionId -> Eff (SessionEff : es) a -> Eff es a
runSessionEff sid = interpret $ \env -> \case
  GetSessionGet         -> pure (newSessionGet sid)
  GetSessionSet         -> pure (newSessionSet sid)
  WithTarget tid action -> localSeqUnlift env \unlift -> do
    oldS <- Session.Pool.get sid
    let oldTid = oldS.currentTargetId
    (newSessionSet sid).currentTarget tid
    unlift action `finally` (newSessionSet sid).currentTarget oldTid


withTarget :: (SessionEff :> es) => TargetId -> Eff es a -> Eff es a
withTarget tid action = send $ WithTarget tid action


get :: (SessionEff :> es, IsFilehub es) => (SessionGet es -> Eff es a) -> Eff es a
get field = do
  viewRecord <- send GetSessionGet
  field viewRecord


set :: (SessionEff :> es, IsFilehub es) => (SessionSet es -> val -> Eff es ()) -> val -> Eff es ()
set field val = do
  setRecord <- send GetSessionSet
  field setRecord val


data SessionGet es = SessionGet
  { currentDir        :: Eff es AbsPath
  , root              :: Eff es AbsPath
  , display           :: Eff es Display
  , sortedFileBy      :: Eff es SortFileBy
  , selected          :: Eff es Selected
  , authId            :: Eff es (Maybe AuthId)
  , sidebarCollapsed  :: Eff es Bool
  , layout            :: Eff es Layout
  , theme             :: Eff es Theme
  , locale            :: Eff es Locale
  , targetViews       :: Eff es [TargetView]
  , controlPanelState :: Eff es (ControlPanelState)
  , sharedLinkPermit  :: Eff es (Maybe SharedLinkPermitSet)
  , oidcFlow          :: Eff es (Maybe SomeOIDCFlow)
  , notifications     :: Eff es (TBQueue Notification)
  , pendingTasks      :: Eff es (TVar (Set TaskId))
  , storage           :: Eff es (Storage (Eff es))
  , currentTarget     :: Eff es TargetView
  }


data SessionSet es = SessionSet
  { currentDir        :: AbsPath -> Eff es ()
  , sortedFileBy        :: SortFileBy -> Eff es  ()
  , selected          :: Selected -> Eff es  ()
  , authId            :: Maybe AuthId -> Eff es ()
  , sidebarCollapsed  :: Bool -> Eff es  ()
  , layout            :: Layout -> Eff es  ()
  , theme             :: Theme -> Eff es ()
  , locale            :: Locale -> Eff es  ()
  , sharedLinkPermit  :: Maybe SharedLinkPermitSet -> Eff es ()
  , currentTarget     :: TargetId -> Eff es ()
  , oidcFlow          :: Maybe SomeOIDCFlow -> Eff es ()
  , notifications     :: TBQueue Notification -> Eff es ()
  , pendingTasks      :: TVar (Set TaskId) -> Eff es ()
  }


newSessionGet :: IsFilehub es => SessionId -> SessionGet es
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
        fromMaybe (pure $ AbsPath "") . asum $
          [ cast tgt <&> \(x :: Target FileSys) -> pure x.root
          , cast tgt <&> \(_ :: Target S3) -> pure (AbsPath "")
          ]


      targetViews = do
        s <- Session.Pool.get sessionId
        let targetIds =  Map.keys s.targets
        targets       <- filter ((`elem` targetIds) . fst) <$> (asks @Env (.targets) >>= readTVarIO)
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
              throwError (FilehubError TargetError "Invalid target")


        fromMaybe onError $ handleTarget t
          [ targetHandler @FileSys \_ -> pure fileStorage
          , targetHandler @S3      \_ -> pure s3Storage
          ]


      currentTarget :: IsFilehub es => Eff es TargetView
      currentTarget = do
        s <- Session.Pool.get sessionId
        targets <- asks @Env (.targets) >>= readTVarIO
        maybe (throwError (FilehubError InvalidSession "Invalid session")) pure do
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


newSessionSet :: IsFilehub es => SessionId -> SessionSet es
newSessionSet sessionId =
  let upS :: IsFilehub es => (Session -> Session) -> Eff es ()
      upS f = Session.Pool.update sessionId f

      upT :: IsFilehub es => (TargetSessionData -> TargetSessionData) -> Eff es ()
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
        runSessionEff sessionId do
          TargetView target _ <- get (.currentTarget)
          targets <- asks @Env (.targets) >>= readTVarIO
          if getTargetId target == tid
             then pure ()
             else do
               case lookup tid targets of
                 Just _ -> do
                   logTrace "[vccxxa] Changing target" (show tid)
                   upS (\s -> s { currentTargetId = tid })
                 Nothing -> do
                   logAttention "[vccxxa] Can't change to target" (show tid)
                   throwError (FilehubError InvalidSession "Invalid session")

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


attachTarget :: IsFilehub es => SessionId -> AnyTarget -> Eff es ()
attachTarget sessionId target = runSessionEff sessionId do
  TargetView current _ <- get (.currentTarget)
  if current == target
     then pure ()
     else do
       Session.Pool.update sessionId \session -> do
         session { targets = Map.insert (getTargetId target) (targetToSessionData target) session.targets
                 }


detachTarget :: IsFilehub es => SessionId -> TargetId -> Eff es ()
detachTarget sessionId targetId = do
  Session.Pool.update sessionId \session -> do
    session { targets = Map.delete targetId session.targets
            }
