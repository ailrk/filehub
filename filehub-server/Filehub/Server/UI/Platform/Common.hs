{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.UI.Platform.Common
  ( sideBar
  )
  where

import Data.ClientPath (AbsPath(..), Root(..))
import Data.DiskUsage (getDiskUsage, DiskUsage(..))
import Filehub.Monad (Filehub)
import Filehub.Session (SessionId, TargetView(..), getTargetViews, getCurrentTarget)
import Filehub.Session.Pool (withSession)
import Filehub.Session.Types (TargetSessionData(..), Selected(..))
import Filehub.Storage.S3 qualified as S3
import Filehub.Template (runTemplate, makeTemplateContext)
import Filehub.Template.Common qualified as Template
import Lucid
import Control.Monad (join)
import Control.Monad.Reader (ask)
import UnliftIO (tryAny, readTVarIO)
import UnliftIO.Async (forConcurrently)
import Target.File (FileSys, Target(..))
import Target.S3 (S3)
import Target.Types (targetHandler, handleTarget)


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  env <- ask
  join $ withSession sessionId \s -> do
    targetViews   <- getTargetViews env s
    currentTarget <- getCurrentTarget env s
    pure do
      ctx      <- makeTemplateContext sessionId
      targets' <- forConcurrently targetViews \(TargetView target td) -> do
        selected' <- readTVarIO td.selected
        let selCount = case selected' of
              Selected _ sels -> length sels + 1
              NoSelection     -> 0
        mUsage <- case handleTarget target
          [ targetHandler @FileSys \(FileBackend { root = Root (AbsPath p) }) -> Left p
          , targetHandler @S3      \s3                                         -> Right s3
          ] of
            Just (Left fsPath) -> getDiskUsage fsPath
            Just (Right s3)    -> do
              result <- tryAny (S3.getBucketSize s3)
              pure $ case result of
                Right used -> Just DiskUsage { total = 0, used }
                Left _     -> Nothing
            Nothing            -> pure Nothing
        pure (target, selCount, mUsage)
      pure $ runTemplate ctx (Template.sideBar targets' currentTarget)
