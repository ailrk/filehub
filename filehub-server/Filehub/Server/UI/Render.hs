{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Utilities for Server.
module Filehub.Server.UI.Render
  ( index
  , view
  , sideBar
  , toolBar
  , controlPanel
  , newFileModal
  , newFolderModal
  , fileDetailModal
  , editorModal
  , renameModal
  , contextMenu
  , entry
  , entries
  , UIPartialUpdate(..)
  , runUIPartialUpdate
  )
  where

import Control.Monad.Reader (MonadReader(ask))
import Data.ClientPath qualified as ClientPath
import Data.File (FileInfo)
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly, ConfirmDesktopOnly)
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Orphan ()
import Filehub.Server.UI.Platform.Desktop qualified as Server.Desktop
import Filehub.Server.UI.Platform.Mobile qualified as Server.Mobile
import Filehub.Server.Util (withQueryParam)
import Filehub.Session (getDisplay, getRoot)
import Filehub.Session.Pool (withSession)
import Filehub.Session.Types (Layout (..))
import Filehub.Template (runTemplate, makeTemplateContext)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Template.Shared qualified as Template
import Filehub.Types
import Lucid hiding ()
import Prelude hiding (elem, readFile, init)
import Prelude hiding (init, readFile)
import Data.Text (Text)
import Lucid.Htmx (HxSwapOOB(..), HxOn (..), Trigger (..))


entry :: SessionId -> FileInfo -> Filehub (Html ())
entry sessionId file = do
  ctx <- makeTemplateContext sessionId
  withSession sessionId \s -> do
    display <- getDisplay s
    pure $ runTemplate ctx
      case display of
        NoDisplay -> Template.Mobile.entry file
        Mobile    -> Template.Mobile.entry file
        Desktop   -> case s.layout of
                       ListLayout      -> Template.Desktop.entry file
                       ThumbnailLayout -> Template.Desktop.thumbnail file


entries :: SessionId -> [FileInfo] -> Filehub (Html ())
entries sessionId files = do
  ctx <- makeTemplateContext sessionId
  withSession sessionId \s -> do
    display <- getDisplay s

    pure $ runTemplate ctx
      case display of
        NoDisplay -> Template.Mobile.entries files
        Mobile    -> Template.Mobile.entries files
        Desktop   -> case s.layout of
                       ListLayout      -> Template.Desktop.entries files
                       ThumbnailLayout -> Template.Desktop.entries files


index :: SessionId -> Filehub (Html ())
index sessionId = do
  display <- withSession sessionId getDisplay
  case display of
    NoDisplay -> pure Template.bootstrap
    Desktop   -> Server.Desktop.index sessionId
    Mobile    -> Server.Mobile.index sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  display <- withSession sessionId getDisplay
  case display of
    Desktop   -> Server.Desktop.view sessionId
    Mobile    -> Server.Mobile.view sessionId
    NoDisplay -> Server.Mobile.view sessionId


controlPanel :: SessionId -> Filehub (Html ())
controlPanel sessionId = do
  display <- withSession sessionId getDisplay
  ctx     <- makeTemplateContext sessionId
  pure $
    case display of
      Desktop -> runTemplate ctx Template.Desktop.controlPanel
      Mobile  -> runTemplate ctx Template.Mobile.controlPanel
      _       -> runTemplate ctx Template.Mobile.controlPanel


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  display <- withSession sessionId getDisplay
  case display of
    Desktop -> Server.Desktop.sideBar sessionId
    _       -> Server.Mobile.sideBar sessionId


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  display <- withSession sessionId getDisplay
  case display of
    Desktop -> Server.Desktop.toolBar sessionId
    _       -> Server.Mobile.toolBar sessionId


renameModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Maybe ClientPath -> Filehub (Html ())
renameModal sessionId _ _ _ mClientPath = do
  root <- withSession sessionId . getRoot =<< ask
  clientPath <- withQueryParam mClientPath
  ctx        <- makeTemplateContext sessionId
  pure $ runTemplate ctx (Template.Desktop.renameModal (ClientPath.fromClientPath root clientPath))


newFileModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Filehub (Html ())
newFileModal sessionId _ _ _ = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.newFileModal


newFolderModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Filehub (Html ())
newFolderModal sessionId  _ _ _ = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.newFolderModal


fileDetailModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> Maybe ClientPath -> Filehub (Html ())
fileDetailModal sessionId _ _ mPath = do
  Server.Desktop.fileDetailModal sessionId mPath


editorModal :: SessionId -> ConfirmLogin -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId _ mClientPath = do
  display <- withSession sessionId getDisplay
  case display of
    Mobile    -> Server.Mobile.editorModal sessionId mClientPath
    Desktop   -> Server.Desktop.editorModal sessionId mClientPath
    NoDisplay -> error "impossible"


contextMenu :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> [ClientPath] -> Filehub (Html ())
contextMenu sessionId _ _ paths = Server.Desktop.contextMenu sessionId paths


data UIPartialUpdate
  = UpdateControlPanel
  | UpdateSideBar
  | UpdateToolBar
  | UpdateEntries [FileInfo]
  | UpdateEntry { target :: Text, file :: FileInfo }
  | UpdateView


runUIPartialUpdate1 :: SessionId -> UIPartialUpdate -> Filehub (Html ())
runUIPartialUpdate1 sessionId = \case
  UpdateControlPanel -> do
    controlPanel' <- controlPanel sessionId
    pure (controlPanel' `with` [ hxSwapOOB True ])

  UpdateSideBar -> do
    sideBar' <- sideBar sessionId
    pure (sideBar' `with` [ hxSwapOOB True ])

  UpdateToolBar -> do
    toolBar' <- toolBar sessionId
    pure (toolBar' `with` [ hxSwapOOB True ])

  UpdateEntries files -> do
    entries' <- entries sessionId files
    pure (entries' `with` [ hxSwapOOB True ])

  UpdateEntry target file -> do
    entry'  <- entry sessionId file
    pure do
      div_  [ hxSwapOOB target ] do
        entry' `with` [ hxOn Load "this.focus();"
                      , tabindex_ "-1" ]

  UpdateView -> do
    view' <- view sessionId
    pure (view' `with` [ hxSwapOOB True ])


runUIPartialUpdate :: SessionId -> [UIPartialUpdate] -> Filehub (Html ())
runUIPartialUpdate sessionId ups = do
  rs <- traverse (runUIPartialUpdate1 sessionId) ups
  pure (mconcat rs)
