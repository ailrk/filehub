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
  , editorModal
  , entry
  , entries
  , UIPartialUpdate(..)
  , runUIPartialUpdate
  )
  where

import Filehub.Prelude
import Data.File (FileInfo)
import Filehub.Handler (ConfirmLogin)
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Orphan ()
import Filehub.Server.UI.Platform.Common qualified as Server.Common
import Filehub.Server.UI.Platform.Desktop qualified as Server.Desktop
import Filehub.Server.UI.Platform.Mobile qualified as Server.Mobile
import Filehub.Session (getDisplay, withSession, Layout(..))
import Filehub.Template (runTemplate, makeTemplateContext)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Template.Common qualified as Template
import Filehub.Types
import Lucid hiding ()
import Prelude hiding (elem, readFile, init)
import Prelude hiding (init, readFile)
import Lucid.Htmx (HxSwapOOB(..), HxOn (..), Trigger (..))
import Control.Monad.Reader (ask)


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
sideBar = Server.Common.sideBar


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  display <- withSession sessionId getDisplay
  case display of
    Desktop -> Server.Desktop.toolBar sessionId
    _       -> Server.Mobile.toolBar sessionId


editorModal :: SessionId -> ConfirmLogin -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId _ mClientPath = do
  display <- withSession sessionId getDisplay
  case display of
    Mobile    -> Server.Mobile.editorModal sessionId mClientPath
    Desktop   -> Server.Desktop.editorModal sessionId mClientPath
    NoDisplay -> error "impossible"


data UIPartialUpdate
  = UpdateControlPanel
  | UpdateSideBar
  | UpdateToolBar
  | UpdateEntries [FileInfo]
  | UpdateEntry { target :: Text, file :: FileInfo }
  | UpdateView
  | UpdatePlugins


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
    rs <- traverse (\f -> (`with` [hxSwapOOB True]) <$> entry sessionId f) files
    pure (mconcat rs)

  UpdateEntry target file -> do
    entry'  <- entry sessionId file
    pure do
      div_  [ hxSwapOOB target ] do
        entry' `with` [ hxOn Load "this.focus();"
                      , tabindex_ "-1" ]

  UpdateView -> do
    view' <- view sessionId
    pure (view' `with` [ hxSwapOOB True ])

  UpdatePlugins -> do
    env     <- ask
    ps      <- readTVarIO env.plugins
    pure (Server.Desktop.pluginsDataEl ps env.pluginJsFiles `with` [ hxSwapOOB True ])


runUIPartialUpdate :: SessionId -> [UIPartialUpdate] -> Filehub (Html ())
runUIPartialUpdate sessionId ups = do
  rs <- traverse (runUIPartialUpdate1 sessionId) ups
  pure (mconcat rs)
