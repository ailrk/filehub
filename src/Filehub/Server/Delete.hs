module Filehub.Server.Delete (delete) where

import Control.Monad (when, void)
import Data.ClientPath (ClientPath (..))
import Data.ClientPath qualified as ClientPath
import Data.Foldable (forM_)
import Data.Ratio ((%))
import Effectful ( raise )
import Effectful.Concurrent.Async (async)
import Effectful.State.Static.Local (modify, get, evalState, execState)
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.Components (index)
import Filehub.Server.Internal qualified as Server.Internal
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Selected
import Filehub.Types (FilehubEvent (..))
import Filehub.Types (Selected (..))
import Lucid ( Html )
import Prelude hiding (init, readFile)
import Servant (addHeader)
import Servant (Headers, Header)
import Target.Types qualified as Target
import Worker.Task (newTaskId)


delete :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> [ClientPath] -> Bool
       -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                            , Header "HX-Trigger" FilehubEvent
                            ] (Html ()))
delete sessionId _ _ clientPaths deleteSelected = do
  count <- Selected.countSelected sessionId
  void $ async do
    taskId <- newTaskId
    storage <- Session.getStorage sessionId
    root    <- Session.getRoot sessionId
    Session.notify sessionId (DeleteProgressed taskId 0)

    nDeleted <- execState @Integer 0 do
      forM_ clientPaths \clientPath -> do
        n <- modify @Integer (+ 1) >> get @Integer
        let path = ClientPath.fromClientPath root clientPath
        raise $ storage.delete path
        Session.notify sessionId (DeleteProgressed taskId (n % max 1 (fromIntegral count)))

    when deleteSelected do
      allSelecteds <- Selected.allSelecteds sessionId
      evalState @Integer nDeleted do
        forM_ allSelecteds \(target, selected) -> do
          Session.withTarget sessionId (Target.getTargetId target) \_ _ -> do
            case selected of
              NoSelection -> pure ()
              Selected x xs -> do
                forM_ (fmap (ClientPath.fromClientPath root) (x:xs)) \path -> do
                  n <- modify @Integer (+ 1) >> get @Integer
                  raise $ storage.delete path
                  Session.notify sessionId (DeleteProgressed taskId (n % max 1 (fromIntegral count)))

    Session.notify sessionId (DeleteProgressed taskId 1)
    Session.notify sessionId (TaskCompleted taskId)
  Server.Internal.clear sessionId
  addHeader count . addHeader SSEStarted <$> index sessionId
