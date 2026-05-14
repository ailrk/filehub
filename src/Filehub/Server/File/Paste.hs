{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.File.Paste (paste) where

import Control.Monad (void)
import Control.Monad.Fix (fix)
import Control.Monad.Reader (MonadReader(..))
import Data.ClientPath (AbsPath (..))
import Data.ClientPath.IO (validateAbsPath)
import Data.Coerce (coerce)
import Data.File (FileType(..), File(..), FileContent (..), withContent, FileInfo, IsLink (..))
import Data.Ratio ((%))
import Data.String.Interpolate (i)
import Data.Traversable (for)
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Server.UI qualified as UI
import Filehub.Session (SessionId(..), TargetView (..), SessionGet(..), withTarget, get, set)
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (CopyState (..))
import Filehub.Types (TargetSessionData (..))
import Log (logAttention_)
import Lucid hiding (for_)
import Lucid.Htmx (HxSwapOOB(..))
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader)
import System.FilePath (takeFileName, (</>))
import Target.Types (AnyTarget)
import UnliftIO (throwIO, newEmptyMVar, takeMVar, putMVar, onException, finally)
import UnliftIO.Async (forConcurrently_)
import UnliftIO.STM (atomically, modifyTVar', readTVar, newTVarIO, writeTBQueue)
import Worker.Task (newTaskId)
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Server.Util (throttle)


data PasteTask
  = PasteFile { from  :: AnyTarget
              , to    :: AnyTarget
              , file  :: FileInfo
              , dst   :: AbsPath
              }
  | CreateDir { to  :: AnyTarget
              , dst :: AbsPath
              }


-- Set the current dir to dir. If an exception happen, this always go back to
-- the original dir.
withDir :: SessionId -> AbsPath -> Filehub a -> Filehub a
withDir sessionId dir action = do
  storage  <- get sessionId (.storage)
  savedDir <- get sessionId (.currentDir)
  storage.cd dir
  action `finally` storage.cd savedDir


-- This function is sequentials, it needs to finish completely to start
-- pasting.
createPasteTasks :: SessionId -> AbsPath -> AnyTarget -> [(AnyTarget, [File FileType])] -> Filehub [PasteTask]
createPasteTasks sessionId fromDir to selections = fmap (mconcat . mconcat) go
  where
    go = do
      for selections \(from, files) -> do
        for files $ flip fix fromDir \rec (AbsPath currentDir) file -> do

          let
              name = coerce takeFileName file.path
              path = (currentDir </> takeFileName name)

          dst <- validateAbsPath path (FilehubError InvalidPath "Invalid path")

          case file.isLink of
            BrokenLink -> pure []
            _          ->
              case file.content of
                Regular    -> pure [ PasteFile from to file dst ]

                Dir -> do
                  withTarget sessionId from do
                    storage <- get sessionId (.storage)
                    withDir sessionId file.path do
                      dirFiles <- storage.lsCwd
                      result <- for dirFiles \dfile -> rec dst dfile
                      pure $ [CreateDir to dst] ++ mconcat result


paste :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
paste sessionId _ _ = do
  notifications   <- get sessionId (.notifications)
  taskId          <- newTaskId
  state           <- get sessionId (.copyState)
  env             <- ask

  TargetView
    pasteTo sdata <- get sessionId (.currentTarget)

  -- States
  pasteCounter    <- newTVarIO @_ @Integer 0
  lk              <- newEmptyMVar

  let
      jot = do
        modifyTVar' pasteCounter (+ 1)
        readTVar pasteCounter

      -- Notify the SSE
      notify n taskCount = do
        writeTBQueue notifications $ PasteProgressed
          { taskId       = taskId
          , progress     = (n % max 1 taskCount)
          , htmxResponse = Nothing
          }

      -- Concurrently paste files
      doPaste tasks taskCount = do
        forConcurrently_ tasks \task -> do
          case task of
            PasteFile { from, to, file, dst } -> do

              conduit <- withTarget sessionId from do
                storage <- get sessionId (.storage)
                storage.readStream file Nothing Nothing

              withTarget sessionId to do
                storage <- get sessionId (.storage)
                storage.write (withContent file (FileContentConduit conduit)) { path = dst }

              atomically do
                n <- jot
                throttle n taskCount do
                  notify n taskCount

            CreateDir to dst -> do
              withTarget sessionId to do
                storage <- get sessionId (.storage)
                void $ storage.newFolder dst

      cleanup = do
        set sessionId (.copyState) NoCopyPaste
        Selected.clearSelectedAllTargets sessionId
        UI.clear sessionId

      handleErr = do
        atomically $
          writeTBQueue notifications $ TaskFailed
            { taskId       = taskId
            , htmxResponse = Nothing
            }

      go selections = do
        tasks <- createPasteTasks sessionId sdata.currentDir pasteTo selections
        let taskCount = fromIntegral (length tasks)
        doPaste tasks taskCount

        view' <- UI.view sessionId
        pure $ Just $ view' `with` [ hxSwapOOB True ]

  case state of
    Paste selections -> forkFilehub_ env do
      _ <- takeMVar lk

      response <- go selections
        `onException` handleErr
        `finally`  cleanup

      atomically do
        writeTBQueue notifications $ TaskCompleted
          { taskId       = taskId
          , htmxResponse = response
          }

    _ -> do
      logAttention_ [i|[v8dsaz] #{sessionId}, not in pastable state.|]
      throwIO (FilehubError SelectError "Not in a pastable state")

  AllSelected{count} <- Selected.getAllSelected sessionId

  htmx <- mkHtmx sessionId
  _    <- putMVar lk ()

  addHeader count <$> pure htmx


mkHtmx :: SessionId -> Filehub (Html ())
mkHtmx sessionId = do
  controlPanel' <- UI.controlPanel sessionId
  sideBar'      <- UI.sideBar sessionId
  pure do
    controlPanel' `with` [ hxSwapOOB True ]
    sideBar' `with` [ hxSwapOOB True ]
