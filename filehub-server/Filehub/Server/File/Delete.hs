{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}

module Filehub.Server.File.Delete (delete) where

import Filehub.Prelude
import Control.Monad.Reader (MonadReader(..))
import Data.ClientPath (ClientPath (..), Root(..), AbsPath(..))
import Data.ClientPath.View (ClientPathView(..), asClientPathView)
import Data.Ratio ((%))
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.History (Operation(..), recordHistory)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..), newTaskId)
import Filehub.Orphan ()
import Filehub.Server.UI qualified as UI
import Filehub.Session (SessionId(..), Session(..), TargetView(..), getRoot, getTargetViews, makeStorageCurrentTarget, makeStorageForTarget, getCurrentTarget)
import Target.Types (AnyTarget(..), getTargetId)
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Session.Types (Selected(..), Storage(..))
import Lucid hiding (for_)
import Lucid.Htmx (HxSwapOOB(..), Swap (..))
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader)
import UnliftIO.STM (writeTBQueue, newTQueueIO, writeTQueue)
import Filehub.Server.Util (throttle)
import Control.Concurrent.STM (flushTQueue)
import UnliftIO (newEmptyMVar, takeMVar, putMVar, finally, onException)
import Filehub.Session.Pool (withSession)
import Filehub.Server.UI.Render (runUIPartialUpdate, UIPartialUpdate (..))
import Target.File (Target(..), FileSys)
import Target.S3 (S3)
import Target.Types (handleTarget, targetHandler)


-- | Delete files.
-- This handler returns immediately, which command the frontend to open a
-- /listen connection. Meanwhile it spawns a new thread running the deletion
-- task. The new thread periodically report progress to the frontend via
-- /listen, and on complete it will send a htmx response that the frontend can
-- use to update the UI.
delete :: SessionId -> ConfirmLogin -> ConfirmReadOnly
       -> [ClientPath] -> Bool
       -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
delete sessionId _ _ clientPaths deleteSelected = do
  taskId    <- newTaskId
  env       <- ask
  withSession sessionId \s -> modifyTVar' s.pendingTasks (set_insert taskId)
  ( notifications
    , root
    , AllSelected { count , allSelected }
    , targetId
    ) <- withSession sessionId \s -> do
    TargetView t _ <- getCurrentTarget env s
    root            <- getRoot env s
    allSelected     <- Selected.getAllSelected =<< getTargetViews env s
    pure ( s.notifications
         , root
         , allSelected
         , getTargetId t
         )

  -- States
  deleteCounter <- newTVarIO @_ @Integer 0
  deletedPaths  <- newTQueueIO @_ @ClientPath
  lk            <- newEmptyMVar

  let total = fromIntegral (count + length clientPaths)

      -- Record on each successful delete.
      jot clientPath = do
        modifyTVar' deleteCounter (+ 1)
        writeTQueue deletedPaths clientPath
        readTVar deleteCounter

      -- Notify the SSE
      notify n = do
        dPaths <- flushTQueue deletedPaths
        let htmx =
              foldMap (\p ->
                let
                    ClientPathView { hashPath } = asClientPathView root p
                 in
                    div_ [ id_ [i|tr-#{hashPath}|], hxSwapOOB Delete ] mempty)
                dPaths
        writeTBQueue notifications $ DeleteProgressed
          { taskId       = taskId
          , progress     = n % max 1 total
          , htmxResponse = Just htmx
          }

      doDelete = do
        -- Delete from parameters
        do
           storage <- makeStorageCurrentTarget sessionId
           forConcurrently_ clientPaths \clientPath -> do
             let ClientPathView { path } = asClientPathView root clientPath
             storage.delete path
             atomically do
               n <- jot clientPath
               throttle n total do
                 notify n

        -- Delete all selected files
        when deleteSelected do
          for_ allSelected \(target, selected) -> do
            storage <- makeStorageForTarget sessionId target

            case selected of
              NoSelection   -> pure ()

              Selected x xs -> do
                let targetRoot = fromMaybe (Root (AbsPath "")) $ handleTarget target
                      [ targetHandler @FileSys \(FileBackend { root = r }) -> r
                      , targetHandler @S3      \_ -> Root (AbsPath "")
                      ]
                    ps = fmap (asClientPathView targetRoot) (x:xs)

                forConcurrently_ ps \(ClientPathView { path, clientPath }) -> do
                  storage.delete path
                  atomically do
                    n <- jot clientPath
                    throttle n total do
                      notify n

      cleanup = do
        UI.clear sessionId

      handleErr = do
        atomically $
          writeTBQueue notifications $ TaskFailed
            { taskId       = taskId
            , htmxResponse = Nothing
            }


  forkFilehub_ env $ do
    _ <- takeMVar lk
    -- Make sure the frontend opens a /listen connection otherwise this will block.
    atomically do
      writeTBQueue notifications $ DeleteProgressed
        { taskId       = taskId
        , progress     = 0
        , htmxResponse = Nothing
        }

    doDelete
      `onException` handleErr
      `finally` cleanup

    when (not (null clientPaths)) do
      recordHistory sessionId (OpDelete targetId clientPaths) True
    when deleteSelected do
      for_ allSelected \(anyTarget, selected) ->
        case selected of
          NoSelection   -> pure ()
          Selected x xs -> recordHistory sessionId (OpDelete (getTargetId anyTarget) (x:xs)) True

    htmxUpdate <- runUIPartialUpdate sessionId [UpdateView, UpdateSideBar]
    atomically do
      writeTBQueue notifications $ TaskCompleted
        { taskId       = taskId
        , htmxResponse = Just htmxUpdate
        }

  UI.clear sessionId

  AllSelected { count = newCount } <- withSession sessionId \s -> do
      Selected.getAllSelected =<< getTargetViews env s

  htmx <- mkHtmx sessionId
  putMVar lk ()

  addHeader newCount <$> pure htmx


mkHtmx :: SessionId -> Filehub (Html ())
mkHtmx sessionId = do
  controlPanel' <- UI.controlPanel sessionId
  sideBar'      <- UI.sideBar sessionId
  pure do
    controlPanel' `with` [ hxSwapOOB True ]
    sideBar' `with` [ hxSwapOOB True ]
