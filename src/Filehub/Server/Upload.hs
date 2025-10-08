module Filehub.Server.Upload (upload) where

import Control.Monad (void)
import Effectful.Concurrent.Async (async)
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.Components (index)
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Lucid ( Html )
import Prelude hiding (init, readFile)
import Servant.Multipart (MultipartData, Mem)
import Worker.Task (newTaskId)


upload :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> MultipartData Mem -> Filehub (Html ())
upload sessionId _ _ multipart = do
  void $ async do
    taskId <- newTaskId
    Session.notify sessionId (UploadProgressed taskId 0)
    storage <- Session.getStorage sessionId
    storage.upload multipart
    Session.notify sessionId (UploadProgressed taskId 1)
    Session.notify sessionId (TaskCompleted taskId)
  index sessionId
