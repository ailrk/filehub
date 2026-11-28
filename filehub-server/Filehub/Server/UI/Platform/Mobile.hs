{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.UI.Platform.Mobile
  ( index
  , editorModal
  , toolBar
  , view
  )
  where

import Filehub.Prelude
import Data.ClientPath (AbsPath(..))
import Filehub.Server.UI.Platform.Common (sideBar)
import Filehub.Server.Util (withQueryParam)
import Filehub.Template (makeTemplateContext, runTemplate, TemplateContext(..))
import Filehub.Session (SessionId, Storage(..), getTargetViews, getRoot, makeStorageCurrentTarget, makeStorageForTarget, targetRoot, fromClientPathChecked, AllSelected(..), withSession)
import Target.Types (TargetId, AnyTarget(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Types (ClientPath, Env(..))
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Filehub.Monad (Filehub)
import Control.Monad.Reader (MonadReader (..))


index :: SessionId -> Filehub (Html ())
index sessionId = do
  env         <- ask
  ctx         <- makeTemplateContext sessionId
  sideBar'    <- sideBar sessionId
  view'       <- view sessionId
  toolBar'    <- toolBar sessionId
  targetViews <- withSession sessionId \s -> getTargetViews env s

  AllSelected {count} <- atomically do
    Selected.getAllSelected targetViews

  pure $ runTemplate ctx (Template.Mobile.index sideBar' toolBar' view' count)


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx (Template.Mobile.toolBar)


editorModal :: SessionId -> Maybe ClientPath -> Maybe TargetId -> Filehub (Html ())
editorModal sessionId mClientPath mTargetId = do
  env <- ask
  ctx <- makeTemplateContext sessionId
  (root, storage) <- case mTargetId of
    Just tid -> do
      targets <- readTVarIO env.targets
      case lookup tid targets of
        Just anyTarget -> do
          s <- makeStorageForTarget sessionId anyTarget
          pure (targetRoot anyTarget, s)
        Nothing -> do
          s <- makeStorageCurrentTarget sessionId
          pure (ctx.root, s)
    Nothing -> do
      r <- withSession sessionId (getRoot env)
      s <- makeStorageCurrentTarget sessionId
      pure (r, s)
  clientPath   <- withQueryParam mClientPath
  p            <- fromClientPathChecked root clientPath
  file         <- storage.get p
  content      <- storage.read file
  let filename = coerce takeFileName p
  pure $ runTemplate ctx (Template.Mobile.editorModal (clientPath, filename) content)


view :: SessionId -> Filehub (Html ())
view sessionId = do
  storage <- makeStorageCurrentTarget sessionId
  ctx@TemplateContext{ sortedBy = order } <- makeTemplateContext sessionId
  table <- do
    files   <- sortFiles order <$> storage.lsCwd
    pure $ runTemplate ctx (Template.Mobile.table files)
  pure $ Template.Mobile.view table
