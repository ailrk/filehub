{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.UI.Mobile
  ( index
  , sideBar
  , editorModal
  , toolBar
  , view
  )
  where

import Data.ClientPath qualified as ClientPath
import Filehub.Env qualified as Env
import Filehub.Server.Util (withQueryParam)
import Filehub.Template (makeTemplateContext, runTemplate, TemplateContext(..))
import Filehub.Session (SessionId, Storage(..), getTargetViews, getCurrentTarget, getRoot, makeStorageDyn)
import Filehub.Session.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Types (ClientPath)
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Data.Coerce (coerce)
import Filehub.Monad (Filehub)
import Control.Monad.Reader (asks, MonadReader (..))
import UnliftIO.STM (readTVar, atomically)
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Session.Pool (withSession)
import Control.Monad (join)


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


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  env         <- ask
  targetsTVar <- asks (.targets)
  withSession sessionId \s -> do
    currentTarget <- getCurrentTarget env s
    targets <- readTVar targetsTVar
    pure $ Template.Mobile.sideBar (fmap snd targets) currentTarget


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
      storage      <- makeStorageDyn sessionId
      ctx          <- makeTemplateContext sessionId
      clientPath   <- withQueryParam mClientPath
      let p        =  ClientPath.fromClientPath root clientPath
      file         <- storage.get p
      content      <- storage.read file
      let filename = coerce takeFileName p
      pure $ runTemplate ctx (Template.Mobile.editorModal (clientPath, filename) content)


view :: SessionId -> Filehub (Html ())
view sessionId = do
  storage <- makeStorageDyn sessionId
  ctx@TemplateContext{ sortedBy = order } <- makeTemplateContext sessionId
  table <- do
    files   <- sortFiles order <$> storage.lsCwd
    pure $ runTemplate ctx (Template.Mobile.table files)
  pure $ Template.Mobile.view table
