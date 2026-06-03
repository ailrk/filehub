{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}

module Filehub.Server.File.Delete (delete) where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import Data.ClientPath (ClientPath (..))
import Data.ClientPath.View (ClientPathView(..), asClientPathView)
import Data.Foldable (for_)
import Data.Ratio ((%))
import Data.String.Interpolate (i)
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.UI qualified as UI
import Filehub.Session (SessionId(..), Session(..), withTarget, getRoot, getStorage, getTargetViews)
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Session.Types (Selected(..), Storage(..))
import Lucid hiding (for_)
import Lucid.Htmx (HxSwapOOB(..), Swap (..))
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader)
import UnliftIO.Async (forConcurrently_)
import UnliftIO.STM (atomically, modifyTVar', readTVar, newTVarIO, writeTBQueue, newTQueueIO, writeTQueue)
import Worker.Task (newTaskId)
import Filehub.Server.Util (throttle)
import Control.Concurrent.STM (flushTQueue)
import UnliftIO (newEmptyMVar, takeMVar, putMVar, finally, onException)
import Filehub.Session.Pool (withSession)


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
  storage   <- getStorage sessionId

  ( notifications
    , root
    , AllSelected { count , allSelected }
    ) <- withSession sessionId \s -> do
    root          <- getRoot env s
    allSelected   <- Selected.getAllSelected <$> getTargetViews env s
    pure ( s.notifications
         , root
         , allSelected
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
        forConcurrently_ clientPaths \clientPath -> do
          let ClientPathView { path } = asClientPathView root clientPath
          storage.delete path
          atomically do
            n <- jot clientPath
            throttle n total do
              notify n

        -- Delete all selected files
        when deleteSelected do
          for_ allSelected \(target, selected) -> withTarget sessionId target do
            case selected of
              NoSelection   -> pure ()

              Selected x xs -> do
                let ps = fmap (asClientPathView root) (x:xs)

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

    atomically do
      writeTBQueue notifications $ TaskCompleted
        { taskId       = taskId
        , htmxResponse = Nothing
        }

  UI.clear sessionId

  AllSelected { count = newCount } <- withSession sessionId \s -> do
      Selected.getAllSelected <$> getTargetViews env s

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
