{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Paste (paste) where

import Control.Monad (forM, void)
import Control.Monad.Fix (fix)
import Data.File (FileType(..), File(..), FileContent (..), withContent, FileInfo)
import Data.Ratio ((%))
import Data.String.Interpolate (i)
import Effectful.Concurrent.Async (async, forConcurrently_)
import Effectful.Concurrent.STM (newTVarIO, readTVar, atomically, modifyTVar', writeTBQueue)
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logAttention_, logTrace)
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.Component (index)
import Filehub.Server.Internal qualified as Server.Internal
import Filehub.Session (SessionId(..), TargetView (..), withTarget, currentTarget)
import Filehub.Session qualified as Session
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import Filehub.Types (CopyState(..), TargetSessionData(..), FilehubEvent(..))
import Lucid (Html)
import Prelude hiding (init, readFile)
import Servant (Headers, Header, addHeader)
import System.FilePath (takeFileName, (</>))
import Target.Types (Target)
import Target.Types qualified as Target
import Worker.Task (newTaskId)


type TargetFrom  = Target
type TargetTo    = Target
type Destination = FilePath


data PasteTask
  = PasteFile TargetFrom TargetTo FileInfo Destination
  | PasteDir TargetTo Destination [PasteTask]


paste :: SessionId -> ConfirmLogin -> ConfirmReadOnly
      -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                           , Header "HX-Trigger" FilehubEvent
                           ] (Html ()))
paste sessionId _ _ = do
  pasteCounter  <- newTVarIO @_ @Integer 0
  taskId        <- newTaskId
  notifications <- Session.getSessionNotifications sessionId
  state         <- Copy.getCopyState sessionId
  case state of
    Paste selections -> do
      tasks <- do
        TargetView to sessionData <- currentTarget sessionId
        createPasteTasks sessionData.currentDir to selections
      let taskCount = fromIntegral (length tasks)

      (void . async) do
        forConcurrently_ tasks $ fix \rec task -> do
          case task of
            PasteFile from to file dst -> do
              let fromId = Target.getTargetId from
              let toId   = Target.getTargetId to
              conduit <- withTarget sessionId fromId \_ storage -> do
                storage.readStream file
              withTarget sessionId toId \_ storage -> do
                storage.write dst (file `withContent` (FileContentConduit conduit))
              atomically do
                modifyTVar' pasteCounter (+ 1)
                n <- readTVar pasteCounter
                writeTBQueue notifications (PasteProgressed taskId (n % max 1 taskCount) )

            PasteDir to dst subTasks -> do
              Session.withTarget sessionId (Target.getTargetId to) \_ storage -> do
                storage.newFolder dst
              forConcurrently_ subTasks rec

        Copy.setCopyState sessionId NoCopyPaste
        Selected.clearSelectedAllTargets sessionId
        atomically $ writeTBQueue notifications (TaskCompleted taskId)
    _ -> do
      logAttention_ [i|[v8dsaz] #{sessionId}, not in pastable state.|]
      throwError (FilehubError SelectError "Not in a pastable state")

  Server.Internal.clear sessionId
  selectedCount <- Selected.countSelected sessionId
  addHeader selectedCount . addHeader SSEStarted <$> index sessionId

  where
    createPasteTasks fromDir to selections = fmap (mconcat . mconcat) do
      forM selections \(from, files) -> do
        forM files $ flip fix fromDir \rec currentDir file -> do
          let dst    = currentDir </> takeFileName file.path
          let fromId = Target.getTargetId from
          case file.content of
            Regular -> do
              pure [ PasteFile from to file dst ]
            Dir -> do
              withTarget sessionId fromId \(TargetView _ (TargetSessionData { currentDir = savedDir })) storage -> do
                storage.cd file.path
                result <- do
                  dirFiles <- storage.lsCwd
                  forM dirFiles \dfile -> do
                    rec dst dfile
                storage.cd savedDir -- go back
                pure [ PasteDir to dst (mconcat result) ]
