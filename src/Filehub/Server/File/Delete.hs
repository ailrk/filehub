{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.File.Delete (delete) where

import Conduit (MonadIO (..))
import Control.Monad (void, when)
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
import Filehub.Session (SessionGet(..))
import Filehub.Session (SessionId(..), withTarget)
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (Selected(..))
import Lucid hiding (for_)
import Lucid.Htmx (HxSwapOOB(..), Swap (..))
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader)
import UnliftIO.Async (async, forConcurrently_)
import UnliftIO.STM (atomically, modifyTVar', readTVar, newTVarIO, writeTBQueue, newTQueueIO, writeTQueue)
import Worker.Task (newTaskId)


-- | Delete files.
-- This handler returns immediately, which command the frontend to open a
-- /listen connection. Meanwhile it spawns a new thread running the deletion
-- task. The new thread periodically report progress to the frontend via
-- /listen, and on complete it will send a htmx response that the frontend can
-- use to update the UI.
delete :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> [ClientPath] -> Bool
       -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
delete sessionId _ _ clientPaths deleteSelected = do
  root            <- Session.get sessionId (.root)
  storage         <- Session.get sessionId (.storage)
  notifications   <- Session.get sessionId (.notifications)
  count           <- length <$> Selected.allSelecteds sessionId
  taskId          <- newTaskId
  deleteCounter   <- newTVarIO @_ @Integer 0
  deleted         <- newTQueueIO @_ @ClientPath
  env             <- ask


  -- Record on each successful delete.
  let jot clientPath = do
        modifyTVar' deleteCounter (+ 1)
        writeTQueue deleted clientPath
        readTVar deleteCounter

  void . async . liftIO . runFilehub env $ do
    -- Make sure the frontend opens a /listen connection otherwise this will block.
    atomically do
      writeTBQueue notifications $ DeleteProgressed
        { taskId       = taskId
        , progress     = 0
        , htmxResponse = Nothing
        }

    -- Delete from parameters
    forConcurrently_ clientPaths \clientPath -> do
      let ClientPathView { path, hashPath } = asClientPathView root clientPath
      storage.delete path
      atomically do
        n <- jot clientPath
        writeTBQueue notifications $ DeleteProgressed
          { taskId       = taskId
          , progress     = n % max 1 (fromIntegral count)
          , htmxResponse = Just $ div_ [ id_ [i|tr-#{hashPath}|], hxSwapOOB Delete ] mempty
          }

    when deleteSelected do
      allSelecteds <- Selected.allSelecteds sessionId
      for_ allSelecteds \(target, selected) -> do
        withTarget sessionId target do
          case selected of
            NoSelection -> pure ()
            Selected x xs -> do
              let ps = fmap (asClientPathView root) (x:xs)
              forConcurrently_  ps \(ClientPathView { path, clientPath, hashPath }) -> do
                storage.delete path
                atomically do
                  n <- jot clientPath
                  writeTBQueue notifications $ DeleteProgressed
                    { taskId       = taskId
                    , progress     = n % max 1 (fromIntegral count)
                    , htmxResponse = Just $ div_ [ id_ [i|tr-#{hashPath}|], hxSwapOOB Delete ] mempty
                    }

    atomically do
      writeTBQueue notifications $ TaskCompleted
        { taskId      = taskId
        , htmxResponse = Nothing
        }

  UI.clear sessionId
  newCount <- length <$> Selected.allSelecteds sessionId
  addHeader newCount
    <$> (do controlPanel' <- UI.controlPanel sessionId
            sideBar'      <- UI.sideBar sessionId
            pure do
              controlPanel' `with` [ hxSwapOOB True ]
              sideBar' `with` [ hxSwapOOB True ])
