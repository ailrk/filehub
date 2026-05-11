{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.File.Paste (paste) where

import Conduit (MonadIO (..))
import Control.Monad (void)
import Control.Monad.Fix (fix)
import Control.Monad.Reader (MonadReader(..))
import Data.ClientPath (AbsPath (..))
import Data.ClientPath.IO (validateAbsPath)
import Data.Coerce (coerce)
import Data.File (FileType(..), File(..), FileContent (..), withContent, FileInfo, IsLink (..))
import Data.Ratio ((%))
import Data.String.Interpolate (i)
import Data.Traversable (for)
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Server.UI qualified as UI
import Filehub.Session (SessionId(..), TargetView (..), SessionGet(..), withTarget)
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (CopyState (..))
import Filehub.Types (TargetSessionData (..))
import Log (logAttention_)
import Lucid hiding (for_)
import Lucid.Htmx (HxSwapOOB(..))
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader)
import System.FilePath (takeFileName, (</>))
import Target.Types (AnyTarget)
import UnliftIO (throwIO)
import UnliftIO.Async (async, forConcurrently_)
import UnliftIO.STM (atomically, modifyTVar', readTVar, newTVarIO, writeTBQueue, newTQueueIO, writeTQueue)
import Worker.Task (newTaskId)


data PasteTask
  = PasteFile { from  :: AnyTarget
              , to    :: AnyTarget
              , file  :: FileInfo
              , dst   :: AbsPath
              }
  | CreateDir { to  :: AnyTarget
              , dst :: AbsPath
              }


createPasteTasks :: SessionId -> AbsPath -> AnyTarget -> [(AnyTarget, [File FileType])] -> Filehub [PasteTask]
createPasteTasks sessionId fromDir to selections = fmap (mconcat . mconcat) do
  for selections \(from, files) -> do
    for files $
      flip fix fromDir \rec (AbsPath currentDir) file -> do

      let name = coerce takeFileName file.path
      let path = (currentDir </> takeFileName name)

      dst <- validateAbsPath path (FilehubError InvalidPath "Invalid path")

      case file.isLink of
        BrokenLink -> pure []
        _          ->
          case file.content of
            Regular    -> pure [ PasteFile from to file dst ]
            Dir -> do
              withTarget sessionId from do
                storage <- Session.get sessionId (.storage)
                (TargetView _ (TargetSessionData { currentDir = savedDir })) <- Session.get sessionId (.currentTarget)
                storage.cd file.path
                result <- do
                  dirFiles <- storage.lsCwd
                  for dirFiles \dfile -> rec dst dfile
                storage.cd savedDir -- go back
                pure $ ([CreateDir to dst] ++ mconcat result)


paste :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
paste sessionId _ _ = do
  notifications   <- Session.get sessionId (.notifications)
  targetViewSaved <- Session.get sessionId (.currentTarget)
  pasteCounter    <- newTVarIO @_ @Integer 0
  taskId          <- newTaskId
  state           <- Session.get sessionId (.copyState)
  pasted          <- newTQueueIO @_ @PasteTask
  env             <- ask

  let jot clientPath = do
        modifyTVar' pasteCounter (+ 1)
        writeTQueue pasted clientPath
        readTVar pasteCounter

  case state of
    Paste selections -> void . async . liftIO . runFilehub env $ do
      tasks <- do
        TargetView to sdata <- Session.get sessionId (.currentTarget)
        createPasteTasks sessionId sdata.currentDir to selections

      let taskCount = fromIntegral (length tasks)

      forConcurrently_ tasks $ \task -> do
        case task of
          PasteFile { from, to, file, dst } -> do

            conduit <- withTarget sessionId from do
              storage <- Session.get sessionId (.storage)
              storage.readStream file Nothing Nothing

            withTarget sessionId to do
              storage <- Session.get sessionId (.storage)
              storage.write (withContent file (FileContentConduit conduit)) { path = dst }

            atomically do
              n <- jot task
              writeTBQueue notifications $ PasteProgressed
                { taskId       = taskId
                , progress     = (n % max 1 taskCount)
                , htmxResponse = Nothing
                }

          CreateDir to dst -> do
            withTarget sessionId to do
              storage <- Session.get sessionId (.storage)
              void $ storage.newFolder dst

      Session.set sessionId (.copyState) NoCopyPaste
      Selected.clearSelectedAllTargets sessionId

      targetView <- Session.get sessionId (.currentTarget)

      response <- do
        if and [ targetView.target == targetViewSaved.target
               , targetView.sessionData.currentDir == targetViewSaved.sessionData.currentDir
               ]
          then do
            view' <- UI.view sessionId
            pure $ Just $ view' `with` [ hxSwapOOB True ]
          else pure Nothing

      atomically do
        writeTBQueue notifications $ TaskCompleted
          { taskId       = taskId
          , htmxResponse = response
          }

    _ -> do
      logAttention_ [i|[v8dsaz] #{sessionId}, not in pastable state.|]
      throwIO (FilehubError SelectError "Not in a pastable state")

  UI.clear sessionId
  selectedCount <- length <$> Selected.allSelecteds sessionId

  addHeader selectedCount
    <$> (do controlPanel' <- UI.controlPanel sessionId
            sideBar'      <- UI.sideBar sessionId
            pure do
              controlPanel' `with` [ hxSwapOOB True ]
              sideBar' `with` [ hxSwapOOB True ])
