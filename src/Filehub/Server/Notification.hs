module Filehub.Server.Notification (listen) where

import Conduit (ConduitT, yield, MonadIO (..), MonadUnliftIO (..))
import Control.Monad (when, join)
import Control.Monad.Fix (fix)
import Data.Set qualified as Set
import Filehub.Handler (ConfirmLogin)
import Filehub.Monad (Filehub)
import Filehub.Notification.Types (Notification(..))
import Filehub.Session (SessionGet(..))
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Prelude hiding (init, readFile)
import Servant.API.EventStream (RecommendedEventSourceHeaders, recommendedEventSourceHeaders)
import UnliftIO.STM (readTBQueue, atomically, isEmptyTBQueue, modifyTVar', readTVar, STM, TBQueue)


type NotificationStream = ConduitT () Notification IO ()


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
-- same conduit; when there are no pending task, the conduit finshes; when there is not task,
-- no conduit.
listen :: SessionId -> ConfirmLogin -> Filehub (RecommendedEventSourceHeaders NotificationStream)
listen sessionId _ = recommendedEventSourceHeaders <$> do
  notifications <- Session.get sessionId (.notifications)
  pendingTasks  <- Session.get sessionId (.pendingTasks)
  streamAtomically \loop -> do
    n <- readTBQueue notifications
    case n of
      TaskCompleted taskId _ -> do
        modifyTVar' pendingTasks (Set.delete taskId)
        tasksRemaining <- readTVar pendingTasks
        if Set.null tasksRemaining
           then do
             clearQueue notifications
             pure (yield n)
           else pure do yield n; loop
      SimpleMessage _        -> pure do yield n; loop
      DeleteProgressed _ _ _ -> pure do yield n; loop
      PasteProgressed _ _ _  -> pure do yield n; loop
      MoveProgressed _ _ _   -> pure do yield n; loop
      UploadProgressed _ _ _ -> pure do yield n; loop
      Pong                   -> pure do yield n; loop


clearQueue :: TBQueue a -> STM ()
clearQueue notifications =
  fix \popMore -> do
    empty <- isEmptyTBQueue notifications
    when (not empty) do
      _ <- readTBQueue notifications
      popMore


streamAtomically :: (NotificationStream -> STM NotificationStream) -> Filehub NotificationStream
streamAtomically action =
  withRunInIO \runInIO -> do
    pure do
      fix \loop -> join . liftIO . runInIO $ atomically do
        action loop
