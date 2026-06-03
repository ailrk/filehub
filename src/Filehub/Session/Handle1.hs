{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Session.Handle1 where

import Conduit (yield)
import Control.Applicative (asum)
import Control.Concurrent.STM (throwSTM)
import Control.Handle.Storage (Storage(..))
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Data.ClientPath (AbsPath (..), Root (..))
import Data.ClientPath (fromClientPath)
import Data.Coerce (coerce)
import Data.File (File (..), FileWithContent, FileContent (..), extractFileInfo)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Typeable (cast)
import Filehub.Display qualified as Display
import Filehub.Error (Error' (..))
import Filehub.Error (FilehubError(..))
import Filehub.Monad (Filehub)
import Filehub.Session.Internal (targetToSessionData)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (TargetView(..), SessionGet (..), SessionSet (..), SessionId, Session, TargetSessionData(..), Session(..), CopyState (..), ControlPanelState (..))
import Filehub.Storage.File qualified as File
import Filehub.Storage.S3 qualified as S3
import Filehub.Types (Display (..), Env(..))
import Filehub.UserAgent qualified as UserAgent
import Log (logAttention_, logAttention)
import Prelude hiding (read, readFile, writeFile)
import Target.File (Target(..), FileSys)
import Target.S3 (S3)
import Target.Types (handleTarget, targetHandler, AnyTarget (..), HasTargetId (..), TargetId)
import UnliftIO (throwIO, finally, writeTVar, atomically, STM, readTVar, TVar)
import UnliftIO.Directory (doesDirectoryExist)


getCurrentTarget :: Env -> Session -> STM TargetView
getCurrentTarget env session = do
  targets  <- readTVar env.targets
  targetId <- readTVar session.currentTargetId

  let mTargetView = do
        targetSessionData <- M.lookup targetId session.targets
        target            <- lookup targetId targets
        pure (TargetView target targetSessionData)

  case mTargetView of
    Just tv -> pure tv
    Nothing -> throwSTM (FilehubError InvalidSession "Invalid session")


getDisplay :: Session -> STM Display
getDisplay session = do
  case session.resolution of
    Just resolution ->
      case session.deviceType of
        UserAgent.Desktop -> pure $ Desktop
        UserAgent.Mobile  -> pure $ Display.classify resolution
        UserAgent.Tablet  -> pure $ Display.classify resolution
        UserAgent.Bot     -> pure $ Display.classify resolution
        UserAgent.Unknown -> pure $ Display.classify resolution
    Nothing -> pure NoDisplay


getRoot :: Env -> Session -> STM Root
getRoot env session = do
  (TargetView (AnyTarget tgt) _) <- getCurrentTarget env session
  fromMaybe (pure $ Root (AbsPath "")) . asum $
    [ cast tgt <&> \(x :: Target FileSys) -> pure x.root
    , cast tgt <&> \(_ :: Target S3) -> pure (Root (AbsPath ""))
    ]


getTargetViews :: Env -> Session -> STM [TargetView]
getTargetViews env session = do
  targets <- readTVar env.targets
  let tvs = flip mapMaybe targets \(targetId, target) ->
              case M.lookup targetId session.targets of
                Just targetData -> pure (TargetView target targetData)
                Nothing         -> Nothing
  pure tvs


getStorage :: Env -> Session -> STM (Storage Filehub)
getStorage env session = do
  (TargetView t _) <- getCurrentTarget env session
  let s3Storage   = makeStorageS3 session
      fileStorage = makeStorageFileSys session
      onError     = do
        throwSTM (FilehubError TargetError "Invalid target")

  fromMaybe onError $ handleTarget t
    [ targetHandler @FileSys \_ -> pure fileStorage
    , targetHandler @S3      \_ -> pure s3Storage
    ]


setCurrentTarget :: Env -> Session -> TargetId -> STM ()
setCurrentTarget env session tid = do
  TargetView target _ <- getCurrentTarget env session
  targets             <-  readTVar env.targets
  if getTargetId target == tid
     then pure ()
     else do
       case lookup tid targets of
         Just _ -> writeTVar session.currentTargetId tid
         Nothing -> do
           throwSTM (FilehubError InvalidSession "Invalid session")



makeStorageS3 :: Session -> Storage Filehub
makeStorageS3 = undefined


makeStorageFileSys :: Session -> Storage Filehub
makeStorageFileSys = undefined
