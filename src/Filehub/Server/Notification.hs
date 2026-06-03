module Filehub.Server.Notification (listen) where

import Conduit (ConduitT, yield, MonadIO (..), MonadUnliftIO (..))
import Control.Monad (when, join)
import Control.Monad.Fix (fix)
import Data.Set qualified as Set
import Filehub.Handler (ConfirmLogin)
import Filehub.Monad (Filehub)
import Filehub.Notification.Types (Notification(..))
import Filehub.Session (SessionId(..))
import Prelude hiding (init, readFile)
import Servant.API.EventStream (RecommendedEventSourceHeaders, recommendedEventSourceHeaders)
import UnliftIO.STM (readTBQueue, atomically, isEmptyTBQueue, modifyTVar', readTVar, STM, TBQueue)
import Filehub.Session.Pool (withSession)
import Filehub.Session (Session(..))


type NotificationStream = ConduitT () Notification IO ()


-- | Creating a notification conduit.
--
-- The conduit tries to read notifications from the `notifications :: TBQueue
-- Notification` and handle each message accordingly.
--
-- == Task
-- If a notification has a task id, it associates with a task that has been
-- created earlier.
--
-- The worker pool reports the progress by sending a notification to this
-- thread. We can then choose to yeild it to downtream or swallow it.
--
-- When a task is completed, a `TaskCompleted` notification will be sent. The
-- session maintains a set of pending task ids, every time we received a
-- `TaskCompleted` message we remove the task Id from the pending task set. We
-- can close the notification if there is no more pending tasks.
--
-- As long as the tab is on, the notification conduit runs forever in the
-- background. When the browser tab is closed, the conduit will fail to yield
-- hence clean up the resource
listen :: SessionId -> ConfirmLogin -> Filehub (RecommendedEventSourceHeaders NotificationStream)
listen sessionId _ = recommendedEventSourceHeaders <$> do
  s <- withSession sessionId pure
  startStream s


startStream :: Session -> Filehub NotificationStream
startStream s =
  streamAtomically \loop -> do
    n <- readTBQueue s.notifications
    case n of
      TaskCompleted taskId _ -> do
        modifyTVar' s.pendingTasks (Set.delete taskId)
        tasksRemaining <- readTVar s.pendingTasks
        if Set.null tasksRemaining
           then do
             clearQueue s.notifications
             pure do yield n; loop
           else pure do yield n; loop
      TaskFailed _ _         -> pure do yield n; loop
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
