module Filehub.Server.Move where

import Control.Monad (when, void)
import Data.ClientPath qualified as ClientPath
import Data.Foldable (forM_)
import Effectful.Concurrent.Async (async)
import Effectful.Error.Dynamic (throwError)
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.Components (index)
import Filehub.Server.Internal qualified as Server.Internal
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Filehub.Types (FilehubEvent (..), MoveFile (..))
import Lucid ( Html )
import Prelude hiding (init, readFile)
import Servant (addHeader)
import Servant (Headers, Header)
import System.FilePath (takeFileName, (</>), takeDirectory)
import Worker.Task (newTaskId)


move :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> MoveFile
     -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent
                          , Header "HX-Trigger" FilehubEvent
                          ] (Html ()))
move sessionId _ _ (MoveFile src tgt) = do
  root         <- Session.getRoot sessionId
  storage      <- Session.getStorage sessionId
  let srcPaths =  fmap (ClientPath.fromClientPath root) src
  let tgtPath  =  ClientPath.fromClientPath root tgt
  void $ async do
    taskId <- newTaskId
    -- check before take action
    forM_ srcPaths \srcPath -> do
      isTgtDir <- storage.isDirectory tgtPath
      when (not isTgtDir) do
        throwError (FilehubError InvalidDir "Target is not a directory")

      when (srcPath == tgtPath)  do
        throwError (FilehubError InvalidDir "Can't move to the same directory")

      when (takeDirectory srcPath == tgtPath)  do
        throwError (FilehubError InvalidDir "Already in the current directory")

    let mvPairs = fmap (\srcPath -> (srcPath, tgtPath </> (takeFileName srcPath))) srcPaths

    Session.notify sessionId (MoveProgressed taskId 0)
    storage.mv mvPairs
    Session.notify sessionId (MoveProgressed taskId 1)
    Session.notify sessionId (TaskCompleted taskId)

  Server.Internal.clear sessionId
  addHeader FileMoved . addHeader SSEStarted <$> index sessionId
