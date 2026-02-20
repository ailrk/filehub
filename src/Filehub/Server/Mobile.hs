{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Mobile
  ( index
  , sideBar
  , editorModal
  , toolBar
  , view
  )
  where

import Data.ClientPath qualified as ClientPath
import Effectful.Reader.Dynamic (asks)
import Filehub.Env (Env)
import Filehub.Env qualified as Env
import Filehub.Monad (IsFilehub)
import Filehub.Server.Internal (withQueryParam)
import Filehub.Template (makeTemplateContext, runTemplate, TemplateContext(..))
import Filehub.Session (SessionId)
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Types (ClientPath)
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Effectful.Error.Dynamic (throwError)
import Filehub.Error (FilehubError(..), Error'(InvalidPath))
import Effectful.Concurrent.STM (readTVarIO)
import Data.Coerce (coerce)
import Effectful (Eff)
import Filehub.Session.Effectful (runSessionEff, SessionGet(..))
import Filehub.Session.Effectful qualified as Session


index :: IsFilehub es => SessionId -> Eff es (Html ())
index sessionId = do
  ctx           <- makeTemplateContext sessionId
  sideBar'      <- sideBar sessionId
  view'         <- view sessionId
  selectedCount <- length <$> Selected.allSelecteds sessionId
  toolBar'      <- toolBar sessionId
  pure $ runTemplate ctx (Template.Mobile.index sideBar' toolBar' view' selectedCount)


sideBar :: IsFilehub es => SessionId -> Eff es (Html ())
sideBar sessionId = runSessionEff sessionId do
  currentTarget <- Session.get (.currentTarget)
  targets <- asks @Env (.targets) >>= readTVarIO
  pure $ Template.Mobile.sideBar (fmap snd targets) currentTarget


toolBar :: IsFilehub es => SessionId -> Eff es (Html ())
toolBar sessionId = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx (Template.Mobile.toolBar)


editorModal :: IsFilehub es => SessionId -> Maybe ClientPath -> Eff es (Html ())
editorModal sessionId mClientPath = runSessionEff sessionId do
  root    <- Session.get (.root)
  storage <- Session.get (.storage)
  ctx        <- makeTemplateContext sessionId
  clientPath <- withQueryParam mClientPath
  let p      =  ClientPath.fromClientPath root clientPath
  mFile <- storage.get p
  case mFile of
    Just file -> do
      content <- storage.read file
      let filename = coerce takeFileName p
      pure $ runTemplate ctx (Template.Mobile.editorModal (clientPath, filename) content)
    Nothing -> do
      throwError (FilehubError InvalidPath "can't edit file")


view :: IsFilehub es => SessionId -> Eff es (Html ())
view sessionId = runSessionEff sessionId do
  storage <- Session.get (.storage)
  ctx@TemplateContext{ sortedBy = order } <- makeTemplateContext sessionId
  table <- do
    files   <- sortFiles order <$> storage.lsCwd
    pure $ runTemplate ctx (Template.Mobile.table files)
  pure $ Template.Mobile.view table
