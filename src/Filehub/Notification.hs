module Filehub.Notification where

import Conduit (ConduitT, yield)
import Control.Monad (join, when)
import Data.Function ((&), fix)
import Data.Set qualified as Set
import Filehub.Error (withServerError)
import Filehub.Monad (Filehub)
import Filehub.Notification.Types (Notification(..))
import Filehub.Session qualified as Session
import Filehub.Session.Types.SessionId (SessionId)
import UnliftIO.STM (readTBQueue, atomically, modifyTVar', readTVar, isEmptyTBQueue)


-- | Creating a notification conduit. The conduit tries to read notifications
-- from the `notifications :: TBQueue Notification` and handle each message accordingly.
--
-- == Task
-- If a notification has a task id, it associates with a task that has been created earlier.
-- The worker pool reports the progress by sending a notification to this thread. We can
-- then choose yeild it to downtream or swallow it.
-- When a task is completed, a `TaskCompleted` notification will be sent. The session
-- maintains a set of pending task ids, every time we received a `TaskCompleted` message
-- we remove the task Id from the pending task set. We can close the notification if there
-- is no more pending tasks.
--
-- This means the notification conduit is created on demand. That is: it's created only when
-- we have a task running in the back ground. When there are multiple tasks, they share the
-- same conduit; when there are no pending task, the conduit finshes; when tehre is not task,
-- no conduit.
notify :: SessionId -> Filehub (ConduitT () Notification IO ())
notify sessionId = do
  notifications <- Session.getSessionNotifications sessionId & withServerError
  pendingTasks  <- Session.getPendingTasks sessionId         & withServerError
  pure $ fix \loop -> join $ atomically do
    notification <- readTBQueue notifications
    case notification of
      TaskCompleted taskId -> do
        modifyTVar' pendingTasks (Set.delete taskId)
        tasksRemaining <- readTVar pendingTasks
        if Set.null tasksRemaining
           then do -- clear notification
             fix \popMore -> do
               empty <- isEmptyTBQueue notifications
               when (not empty) do
                _ <- readTBQueue notifications
                popMore
             pure do
              yield notification
           else
           pure do
             yield notification
             loop
      DeleteProgressed _ _ -> pure do yield notification >> loop
      PasteProgressed _ _  -> pure do yield notification >> loop
      MoveProgressed _ _   -> pure do yield notification >> loop
      UploadProgressed _ _ -> pure do yield notification >> loop
      Pong                 -> pure do yield notification >> loop
