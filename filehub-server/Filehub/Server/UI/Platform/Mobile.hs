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
import Filehub.Session (SessionId, Storage(..), getTargetViews, getRoot, makeStorageCurrentTarget, fromClientPathChecked)
import Filehub.Session.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Types (ClientPath)
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Filehub.Monad (Filehub)
import Control.Monad.Reader (MonadReader (..))
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Session.Pool (withSession)


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


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath = do
  env <- ask
  join $ withSession sessionId \s -> do
    root    <- getRoot env s
    pure do
      storage      <- makeStorageCurrentTarget sessionId
      ctx          <- makeTemplateContext sessionId
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
