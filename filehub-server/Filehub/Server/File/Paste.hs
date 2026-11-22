{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.File.Paste (paste) where

import Control.Monad (void)
import Control.Monad.Fix (fix)
import Control.Monad.Reader (MonadReader(..))
import Data.ClientPath (AbsPath (..), toClientPath)
import Data.Hashable (hash)
import Data.Text qualified as T
import Data.ClientPath.IO (validateAbsPath)
import Data.Coerce (coerce)
import Data.File (FileType(..), File(..), FileContent (..), withContent, FileInfo, IsLink (..))
import Data.Maybe (catMaybes)
import Data.Ratio ((%))
import Data.String.Interpolate (i)
import Data.Traversable (for)
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.History (Operation(..), recordHistory)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Server.UI qualified as UI
import Filehub.Server.Util (throttle)
import Data.ClientPath qualified as ClientPath
import Filehub.Session (Session(..), makeStorageForTarget, targetRoot)
import Filehub.Session (SessionId(..), TargetView (..), getCurrentTarget, getTargetViews, getTarget, makeStorage)
import Filehub.Session.Pool (withSession, withSession_)
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (CopyState (..), Storage(..))
import Filehub.Types (TargetSessionData (..))
import Log (logAttention_)
import Lucid hiding (for_)
import Lucid.Htmx (HxSwapOOB(..))
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader)
import System.FilePath (takeFileName, (</>))
import Target.Types (AnyTarget, getTargetId)
import UnliftIO (throwIO, newEmptyMVar, takeMVar, putMVar, finally, readTVarIO, catch, SomeException)
import UnliftIO.Async (pooledForConcurrentlyN_)
import UnliftIO.STM (atomically, modifyTVar', readTVar, newTVarIO, writeTBQueue)
import Worker.Task (newTaskId)
import UnliftIO.Concurrent (getNumCapabilities)
import Data.Set qualified as Set


data PasteTask
  = PasteFile { from     :: AnyTarget
              , to       :: AnyTarget
              , file     :: FileInfo
              , dst      :: AbsPath
              }
  | CreateDir { to       :: AnyTarget
              , dst      :: AbsPath
              , subTasks :: [PasteTask]
              }


countTasks :: [PasteTask] -> Integer
countTasks ts = sum (map go ts)
  where
    go PasteFile{}            = 1
    go CreateDir{ subTasks }  = 1 + countTasks subTasks


-- Set the current dir to dir. If an exception happens, this always go back to
-- the original dir.
withDir :: TargetView -> AbsPath -> (Storage Filehub -> Filehub a) -> Filehub a
withDir tv@(TargetView _ td) dir action = do
  storage  <- makeStorage tv
  savedDir <- readTVarIO td.currentDir
  storage.cd dir
  action storage `finally` storage.cd savedDir


-- This function is sequential, it needs to finish completely to start pasting.
createPasteTasks :: SessionId -> AbsPath -> AnyTarget -> [(AnyTarget, [File FileType])] -> Filehub [PasteTask]
createPasteTasks sessionId fromDir to selections = fmap mconcat go
  where
    go = do
      env <- ask
      for selections \(from, files) -> do
        tasks <- for files $ flip fix fromDir \rec (AbsPath currentDir) file -> do

          let
              name = coerce takeFileName file.path
              path = (currentDir </> takeFileName name)

          dst <- validateAbsPath path (FilehubError InvalidPath "Invalid path")

          case file.isLink of
            BrokenLink -> pure Nothing
            _          ->
              case file.content of
                Regular    -> pure (Just (PasteFile from to file dst))

                Dir -> do
                  targetView <- withSession sessionId \s -> getTarget env s from
                  withDir targetView file.path \storage -> do
                    dirFiles <- storage.lsCwd
                    result   <- for dirFiles \dfile -> rec dst dfile
                    pure $ Just (CreateDir to dst (catMaybes result))
        pure $ catMaybes tasks


paste :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
paste sessionId _ _ = do
  env    <- ask
  taskId <- newTaskId
  nCores <- getNumCapabilities
  withSession sessionId \s -> modifyTVar' s.pendingTasks (Set.insert taskId)

  ( notifications
    , state
    , TargetView pasteTo sdata
    ) <- withSession sessionId \s -> do

    currentTarget <- getCurrentTarget env s

    pure ( s.notifications
         , s.copyState
         , currentTarget
         )

  -- States
  pasteCounter <- newTVarIO @_ @Integer 0
  lk           <- newEmptyMVar

  let
      jot = do
        modifyTVar' pasteCounter (+ 1)
        readTVar pasteCounter

      -- Notify the SSE
      notify n totalCount = do
        writeTBQueue notifications $ PasteProgressed
          { taskId       = taskId
          , progress     = (n % max 1 totalCount)
          , htmxResponse = Nothing
          }

      -- Concurrently paste files
      doPaste tasks totalCount = do
        pooledForConcurrentlyN_ (nCores * 3) tasks \task -> do
          case task of
            PasteFile { from, to, file, dst } -> do

              conduit <- do
                storage <- makeStorageForTarget sessionId from
                storage.readStream file Nothing Nothing

              do
                storage <- makeStorageForTarget sessionId to
                storage.write (withContent file (FileContentConduit conduit)) { path = dst }

              atomically do
                n <- jot
                throttle n totalCount do
                  notify n totalCount

            CreateDir { to, dst, subTasks } -> do
              storage <- makeStorageForTarget sessionId to
              void $ storage.newFolder dst
              doPaste subTasks totalCount

      cleanup = do
        withSession_ sessionId \s -> pure (s { copyState = NoCopyPaste } , ())
        Selected.clearSelectedAllTargets sessionId
        UI.clear sessionId

      handleErr (e :: SomeException) = do
        atomically $
          writeTBQueue notifications $ TaskFailed
            { taskId       = taskId
            , htmxResponse = Nothing
            }
        throwIO e

      go selections = do
        currentDir' <- readTVarIO sdata.currentDir
        tasks       <- createPasteTasks sessionId currentDir' pasteTo selections
        doPaste tasks (countTasks tasks)

        view'    <- UI.view sessionId
        sideBar' <- UI.sideBar sessionId

        let root      = targetRoot pasteTo
            hashPaths = [ show (fromIntegral @_ @Word (hash cp))
                        | (_, files) <- selections
                        , f          <- files
                        , let cp = toClientPath root (AbsPath (coerce currentDir' </> takeFileName (coerce f.path)))
                        ]
            idsJs     = T.intercalate "," (map (\h -> [i|'tr-#{h}'|]) hashPaths)
            hlScript  = [i|requestAnimationFrame(function(){[#{idsJs}].forEach(function(id){var el=document.getElementById(id);if(el){el.classList.add('pasted-new');}});});|] :: T.Text
            hlTrigger = div_ [ id_ "paste-hl", hxSwapOOB True ] do
                          script_ hlScript

        pure $ Just do
          view'    `with` [ hxSwapOOB True ]
          sideBar' `with` [ hxSwapOOB True ]
          hlTrigger

  case state of
    Paste selections -> forkFilehub_ env do
      _ <- takeMVar lk

      response <- go selections
        `catch` handleErr
        `finally`  cleanup

      currentDir' <- readTVarIO sdata.currentDir
      let fileNames = [ T.pack (takeFileName (coerce f.path))
                      | (_, files) <- selections, f <- files ]
          dstCP = ClientPath.toClientPath (targetRoot pasteTo) currentDir'
      recordHistory sessionId (OpPaste (getTargetId pasteTo) dstCP fileNames) True

      withSession sessionId \s -> do
        TargetView currentTarget _ <- getCurrentTarget env s
        writeTBQueue notifications $ TaskCompleted
          { taskId       = taskId
          , htmxResponse = if currentTarget == pasteTo
                              then response
                              else Nothing
          }

    _ -> do
      logAttention_ [i|[v8dsaz] #{sessionId}, not in pastable state.|]
      throwIO (FilehubError SelectError "Not in a pastable state")

  Copy.clearCopyState sessionId
  UI.clear sessionId

  AllSelected { count } <- withSession sessionId \s -> do
    Selected.getAllSelected =<< getTargetViews env s

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
