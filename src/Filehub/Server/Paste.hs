module Filehub.Server.Paste (paste) where

import Control.Monad (void)
import Data.File (FileType(..), File(..), FileContent (..), withContent)
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Ratio ((%))
import Data.String.Interpolate (i)
import Effectful.Concurrent.Async (async)
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logAttention_)
import Effectful.State.Static.Local (modify, get, evalState)
import Filehub.Error ( withServerError, FilehubError(..), withServerError, Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.Components (index)
import Filehub.Server.Internal qualified as Server.Internal
import Filehub.Session (SessionId(..), TargetView (..))
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


paste :: SessionId -> ConfirmLogin -> ConfirmReadOnly
      -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                           , Header "HX-Trigger" FilehubEvent
                           ] (Html ()))
paste sessionId _ _ = do
  count <- Selected.countSelected sessionId & withServerError
  taskId <- newTaskId
  withServerError do
    state <- Copy.getCopyState sessionId
    case state of
      Paste selections -> do
        void $ async do
          TargetView to sessionData _ <- Session.currentTarget sessionId
          Session.notify sessionId (PasteProgressed taskId 0)
          evalState @Integer 0 do
            forM_ selections \(from, files) -> do
              forM_ files \file -> do
                goPaste from to sessionData.currentDir file
                n <- modify @Integer (+ 1) >> get @Integer
                Session.notify sessionId (PasteProgressed taskId (n % max 1 (fromIntegral count)))
          Copy.setCopyState sessionId NoCopyPaste
          Selected.clearSelectedAllTargets sessionId
          Session.notify sessionId (PasteProgressed taskId 1)
          Session.notify sessionId (TaskCompleted taskId)
        pure ()
      _ -> do
        logAttention_ [i|Paste error: #{sessionId}, not in pastable state.|]
        throwError (FilehubError SelectError "Not in a pastable state")
  Server.Internal.clear sessionId
  addHeader count . addHeader SSEStarted <$> index sessionId
  where
    goPaste from to currentDir file = do
      case file.content of
        Regular -> do
          conduit <- Session.withTarget sessionId (Target.getTargetId from) \_ storage -> do
            storage.readStream file
          Session.withTarget sessionId (Target.getTargetId to) \_ storage -> do
            storage.write (currentDir </> takeFileName file.path) (file `withContent` (FileContentConduit conduit))
        Dir -> do -- copy directory layer by layer
          dst <- Session.withTarget sessionId (Target.getTargetId to) \_ storage -> do
            let dst = currentDir </> takeFileName file.path
            storage.newFolder dst
            pure dst
          Session.withTarget sessionId (Target.getTargetId from)
            \(TargetView _ (TargetSessionData { currentDir = savedDir }) _) storage -> do
              storage.cd file.path
              do
                dirFiles <- storage.lsCwd
                forM_ dirFiles (goPaste from to dst)
              storage.cd savedDir -- go back
