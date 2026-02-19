module Filehub.Server.Upload (upload) where

import Control.Monad (void)
import Data.Ratio ((%))
import Effectful.Concurrent.Async (async, forConcurrently_)
import Effectful.Concurrent.STM (atomically, writeTBQueue, newTVarIO, modifyTVar', readTVar)
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.Component (index)
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Filehub.Types (FilehubEvent(..))
import Lucid ( Html )
import Prelude hiding (init, readFile)
import Servant (addHeader, Headers, Header)
import Servant.Multipart (MultipartData(..), Mem)
import Worker.Task (newTaskId)


upload :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> MultipartData Mem
       -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent
                            ] (Html ()))
upload sessionId _ _ multipart = do
  notifications <- Session.getSessionNotifications sessionId
  taskId        <- newTaskId
  uploadCounter <- newTVarIO @_ @Integer 0
  let taskCount =  fromIntegral $ length multipart.files

  void $ async do
    atomically do
      writeTBQueue notifications (UploadProgressed taskId 0)

    Session.withStorage sessionId \storage -> do
      forConcurrently_ multipart.files \filedata -> do
        storage.upload filedata
        atomically do
          modifyTVar' uploadCounter (+ 1)
          n <- readTVar uploadCounter
          writeTBQueue notifications (UploadProgressed taskId (n % max 1 taskCount) )

      atomically do
        writeTBQueue notifications (UploadProgressed taskId 1)
        writeTBQueue notifications (TaskCompleted taskId)
  addHeader SSEStarted <$> index sessionId
