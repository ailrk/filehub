module Filehub.Server.Delete (delete) where

import Control.Monad (when, void)
import Data.ClientPath (ClientPath (..))
import Data.ClientPath qualified as ClientPath
import Data.Foldable (forM_)
import Data.Ratio ((%))
import Effectful.Concurrent.Async (async, mapConcurrently_)
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.Component (index)
import Filehub.Server.Internal qualified as Server.Internal
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Selected
import Filehub.Types ( FilehubEvent(..), Selected(..) )
import Lucid ( Html )
import Prelude hiding (init, readFile)
import Servant ( addHeader, Headers, Header )
import Target.Types qualified as Target
import Worker.Task (newTaskId)
import Effectful.Concurrent.STM (newTVarIO, modifyTVar', atomically, readTVar, writeTBQueue)


delete :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> [ClientPath] -> Bool
       -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                            , Header "HX-Trigger" FilehubEvent
                            ] (Html ()))
delete sessionId _ _ clientPaths deleteSelected = do
  count         <- length <$> Selected.allSelecteds sessionId
  root          <- Session.getRoot sessionId
  storage       <- Session.getStorage sessionId
  taskId        <- newTaskId
  deleteCounter <- newTVarIO @_ @Integer 0
  notifications <- Session.getSessionNotifications sessionId

  void $ async do
    atomically do writeTBQueue notifications (DeleteProgressed taskId 0)

    do
      flip mapConcurrently_ clientPaths \clientPath -> do
        let path = ClientPath.fromClientPath root clientPath
        storage.delete path
        atomically do
          modifyTVar' deleteCounter (+ 1)
          n <- readTVar deleteCounter
          writeTBQueue notifications (DeleteProgressed taskId (n % max 1 (fromIntegral count)))

    when deleteSelected do
      allSelecteds <- Selected.allSelecteds sessionId
      forM_ allSelecteds \(target, selected) -> do
        Session.withTarget sessionId (Target.getTargetId target) \_ _ -> do
          case selected of
            NoSelection -> pure ()
            Selected x xs -> do
              flip mapConcurrently_ (fmap (ClientPath.fromClientPath root) (x:xs)) \path -> do
                storage.delete path
                atomically do
                  modifyTVar' deleteCounter (+ 1)
                  n <- readTVar deleteCounter
                  writeTBQueue notifications (DeleteProgressed taskId (n % max 1 (fromIntegral count)))

    atomically do writeTBQueue notifications (TaskCompleted taskId)
  Server.Internal.clear sessionId
  newCount <- length <$> Selected.allSelecteds sessionId
  addHeader newCount . addHeader SSEStarted <$> index sessionId
