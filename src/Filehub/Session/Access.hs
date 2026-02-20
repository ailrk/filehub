{-# LANGUAGE RankNTypes #-}
module Filehub.Session.Access where

import Data.ClientPath (AbsPath (..))
import Filehub.Monad (Filehub)
import Filehub.Display (Display (..))
import Filehub.Sort (SortFileBy)
import Filehub.Auth.Types (AuthId)
import Filehub.Types (Layout, ControlPanelState (..), Env(..), CopyState (..), Selected)
import Filehub.Theme (Theme)
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
import Filehub.Error (FilehubError(FilehubError), Error' (..))
import Data.Map qualified as Map
import Filehub.UserAgent qualified as UserAgent
import Filehub.Display qualified as Display
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
import Effectful.Log (logAttention_)
import Target.Types (handleTarget, targetHandler, AnyTarget (..))


data SessionView = SessionView
  { currentDir        :: ~AbsPath
  , display           :: ~Display
  , root              :: ~AbsPath
  , sortFileBy        :: ~SortFileBy
  , selected          :: ~Selected
  , authId            :: ~(Maybe AuthId)
  , sidebarCollapsed  :: ~Bool
  , layout            :: ~Layout
  , theme             :: ~Theme
  , locale            :: ~Locale
  , targetViews       :: ~[TargetView]
  , controlPanelState :: ~ControlPanelState
  , sharedLinkPermit  :: ~(Maybe SharedLinkPermitSet)
  , currentTarget     :: ~TargetView
  , notifications     :: ~(TBQueue Notification)
  , pendingTasks      :: ~(TVar (Set TaskId))
  , storage           :: ~(Storage Filehub)
  }


data SessionUpdate
  = SetCurrentDir AbsPath
  | SetDisplay Display
  | SetSortFileBy SortFileBy
  | SetSelected Selected
  | SetAuthId (Maybe AuthId)
  | ToggleSidebarCollapse
  | SetLayout Layout
  | SetTheme Theme
  | SetSharedLinkPermit (Maybe SharedLinkPermitSet)


updateSession :: SessionUpdate -> Filehub ()
updateSession = \case
  SetCurrentDir p -> undefined
  SetDisplay d -> undefined
  SetSortFileBy s -> undefined
  SetSelected s -> undefined
  SetAuthId a -> undefined
  ToggleSidebarCollapse -> undefined
  SetLayout l -> undefined
  SetTheme t -> undefined
  SetSharedLinkPermit s -> undefined


viewSession :: SessionId -> Filehub SessionView
viewSession sessionId = do
  s <- Session.Pool.get sessionId
  c@(TargetView t@(AnyTarget tgt) td) <- currentTarget s


  let display = case s.resolution of
                  Just resolution ->
                    case s.deviceType of
                      UserAgent.Desktop -> Desktop
                      UserAgent.Mobile  -> Display.classify resolution
                      UserAgent.Tablet  -> Display.classify resolution
                      UserAgent.Bot     -> Display.classify resolution
                      UserAgent.Unknown -> Display.classify resolution
                  Nothing -> NoDisplay


  let root = fromMaybe (AbsPath "") . asum $
        [ cast tgt <&> \(x :: Target FileSys) -> x.root
        , cast tgt <&> \(_ :: Target S3) -> AbsPath ""
        ]


  targetViews <- do
    let targetIds   =  Map.keys s.targets
    targets         <- filter ((`elem` targetIds) . fst) <$> (asks @Env (.targets) >>= readTVarIO)
    let targetViews =  flip mapMaybe targets \(targetId, target) ->
          case Map.lookup targetId s.targets of
            Just targetData -> pure (TargetView target targetData)
            Nothing         -> Nothing
    pure targetViews


  controlPanelState <- do
    isAnySelected <- Selected.anySelected sessionId
    copyState     <- Copy.getCopyState sessionId
    case (isAnySelected, copyState) of
      (_, Paste {})           -> pure ControlPanelCopied
      (True, CopySelected {}) -> pure ControlPanelSelecting
      (True, NoCopyPaste)     -> pure ControlPanelSelecting
      _                       -> pure ControlPanelDefault


  storage <- do
    let s3Storage   = S3.storage sessionId
        fileStorage = File.storage sessionId
        onError     = do
          logAttention_ "[ssshuu] Target error"
          throwError (FilehubError TargetError "Invalid target")


    fromMaybe onError $ handleTarget t
      [ targetHandler @FileSys \_ -> pure fileStorage
      , targetHandler @S3      \_ -> pure s3Storage
      ]


  pure SessionView
    { currentDir        = td.currentDir
    , display           = display
    , root              = root
    , sortFileBy        = td.sortedFileBy
    , selected          = td.selected
    , authId            = s.authId
    , sidebarCollapsed  = s.sidebarCollapsed
    , layout            = s.layout
    , theme             = s.theme
    , locale            = s.locale
    , targetViews       = targetViews
    , controlPanelState = controlPanelState
    , sharedLinkPermit  = s.sharedLinkPermit
    , currentTarget     = c
    , notifications     = s.notifications
    , pendingTasks      = s.pendingTasks
    , storage           = storage
    }


currentTarget :: Session -> Filehub TargetView
currentTarget s = do
  targets <- asks @Env (.targets) >>= readTVarIO
  maybe (throwError (FilehubError InvalidSession "Invalid session")) pure do
    let targetId      = s.currentTargetId
    targetSessionData <- Map.lookup targetId s.targets
    target            <- lookup targetId targets
    pure $ TargetView target targetSessionData
