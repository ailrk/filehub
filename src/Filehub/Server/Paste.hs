{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Paste (paste) where

import Control.Monad (forM, void)
import Data.File (FileType(..), File(..), FileContent (..), withContent, FileInfo)
import Data.Function ((&))
import Data.Ratio ((%))
import Data.String.Interpolate (i)
import Effectful.Concurrent.Async (async, mapConcurrently_)
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logAttention_)
import Filehub.Error ( withServerError, FilehubError(..), withServerError, Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.Components (index)
import Filehub.Server.Internal qualified as Server.Internal
import Filehub.Session (SessionId(..), TargetView (..), withTarget, notify, currentTarget)
import Filehub.Session qualified as Session
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import Filehub.Types (CopyState (..), TargetSessionData (..))
import Filehub.Types (FilehubEvent (..))
import Lucid ( Html )
import Prelude hiding (init, readFile)
import Servant (Headers, Header)
import Servant (addHeader)
import System.FilePath (takeFileName, (</>))
import Target.Types qualified as Target
import Worker.Task (newTaskId)
import Control.Monad.Fix (fix)
import Target.Types (Target)
import Effectful.Concurrent.STM (newTVarIO, readTVar, atomically, modifyTVar', writeTBQueue)
import Debug.Trace


data PasteTask = PasteTask
  { from :: Target
  , to :: Target
  , file :: FileInfo
  , dst :: FilePath
  }


paste :: SessionId -> ConfirmLogin -> ConfirmReadOnly
      -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                           , Header "HX-Trigger" FilehubEvent
                           ] (Html ()))
paste sessionId _ _ = do
  selectedCount <- Selected.countSelected sessionId & withServerError
  taskId        <- newTaskId
  pasteCounter  <- newTVarIO @_ @Integer 0

  withServerError do
    state <- Copy.getCopyState sessionId
    notifications <- Session.getSessionNotifications sessionId

    case state of
      Paste selections -> do
        -- totalPasteCount <- countTotalFiles selections
        TargetView to sessionData _ <- currentTarget sessionId
        tasks <- createPasteTasks sessionData.currentDir to selections
        let taskCount = fromIntegral (length tasks)

        (void . async) do
          mapConcurrently_ (runTask taskId notifications pasteCounter taskCount) tasks
          Copy.setCopyState sessionId NoCopyPaste
          Selected.clearSelectedAllTargets sessionId
          notify sessionId (TaskCompleted taskId)
      _ -> do
        logAttention_ [i|Paste error: #{sessionId}, not in pastable state.|]
        throwError (FilehubError SelectError "Not in a pastable state")

  Server.Internal.clear sessionId
  addHeader selectedCount. addHeader SSEStarted <$> index sessionId

  where
    runTask taskId notifications counterTVar totalPasteCount PasteTask { from, to, file, dst } = do
      traceShowM ("task " <> file.path <> " " <> dst)
      let fromId = Target.getTargetId from
      let toId   = Target.getTargetId to
      conduit <- withTarget sessionId fromId \_ storage -> do
        storage.readStream file
      withTarget sessionId toId \_ storage -> do
        storage.write dst (file `withContent` (FileContentConduit conduit))
      atomically do
        modifyTVar' counterTVar (+ 1)
        n <- readTVar counterTVar
        writeTBQueue notifications (PasteProgressed taskId (n % max 1 totalPasteCount) )

    createPasteTasks fromDir to selections = fmap (mconcat . mconcat) do
      forM selections \(from, files) -> do
        forM files $ flip fix fromDir \rec currentDir file -> do
          let dst    = currentDir </> takeFileName file.path
          let fromId = Target.getTargetId from
          case file.content of
            Regular -> do
              pure [ PasteTask { from, to, file, dst } ]
            Dir -> do
              withTarget sessionId fromId \(TargetView _ (TargetSessionData { currentDir = savedDir }) _) storage -> do
                storage.cd file.path
                result <- do
                  dirFiles <- storage.lsCwd
                  forM dirFiles \dfile -> do
                    rec dst dfile
                storage.cd savedDir -- go back
                pure (mconcat result)
