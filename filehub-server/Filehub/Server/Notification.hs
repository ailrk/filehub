{-# LANGUAGE NumericUnderscores #-}
module Filehub.Server.Notification (listen) where

import Filehub.Prelude
import Conduit (ConduitT, yield)
import Filehub.Handler (ConfirmLogin)
import Filehub.Monad (Filehub)
import Filehub.Notification.Types (Notification(..))
import Filehub.Session (SessionId(..), Session(..), withSession)
import Control.Concurrent.STM (retry)
import GHC.Conc (registerDelay)
import Prelude hiding (init, readFile)
import Servant.API.EventStream (RecommendedEventSourceHeaders, recommendedEventSourceHeaders)
import UnliftIO.STM (readTBQueue, isEmptyTBQueue, TBQueue, orElse)


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
-- hence clean up the resource.
--
-- == Heartbeat
-- A Pong is emitted every `pingInterval` microseconds when the queue is idle.
-- This prevents reverse-proxy idle-timeout from silently dropping the
-- connection (which the browser sees as a clean close and does not reconnect).
listen :: SessionId -> ConfirmLogin -> Filehub (RecommendedEventSourceHeaders NotificationStream)
listen sessionId _ = recommendedEventSourceHeaders <$> do
  s <- withSession sessionId pure
  startStream s


startStream :: Session -> Filehub NotificationStream
startStream s = withRunInIO \runInIO -> pure $ fix \loop -> do
  timer  <- liftIO $ registerDelay pingInterval
  result <- liftIO . atomically $
    (Just <$> readTBQueue s.notifications)
    `orElse`
    (readTVar timer >>= \fired -> if fired then pure Nothing else retry)
  case result of
    Nothing -> yield Pong >> loop
    Just n  -> join . liftIO . runInIO $ atomically (handleN n loop)
  where
    pingInterval = 25_000_000  -- 25 seconds

    handleN n loop = case n of
      TaskCompleted taskId _ -> do
        modifyTVar' s.pendingTasks (set_delete taskId)
        tasksRemaining <- readTVar s.pendingTasks
        if set_null tasksRemaining
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
