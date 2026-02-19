{-# LANGUAGE PartialTypeSignatures #-}
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
import Filehub.Monad ( Filehub )
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


index :: SessionId -> Filehub (Html ())
index sessionId = do
  ctx           <- makeTemplateContext sessionId
  sideBar'      <- sideBar sessionId
  view'         <- view sessionId
  selectedCount <- length <$> Selected.allSelecteds sessionId
  toolBar'      <- toolBar sessionId
  pure $ runTemplate ctx (Template.Mobile.index sideBar' toolBar' view' selectedCount)


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  targets <- asks @Env (.targets) >>= readTVarIO
  Template.Mobile.sideBar (fmap snd targets) <$> Session.currentTarget sessionId


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx (Template.Mobile.toolBar)


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath =
  Session.withStorage sessionId \storage -> do
    ctx        <- makeTemplateContext sessionId
    clientPath <- withQueryParam mClientPath
    root       <- Session.getRoot sessionId
    let p      =  ClientPath.fromClientPath root clientPath
    mFile <- storage.get p
    case mFile of
      Just file -> do
        content <- storage.read file
        let filename = coerce takeFileName p
        pure $ runTemplate ctx (Template.Mobile.editorModal (clientPath, filename) content)
      Nothing -> do
        throwError (FilehubError InvalidPath "can't edit file")


view :: SessionId -> Filehub (Html ())
view sessionId =
  Session.withStorage sessionId \storage -> do
    ctx@TemplateContext{ sortedBy = order } <- makeTemplateContext sessionId
    table <- do
      files   <- sortFiles order <$> storage.lsCwd
      pure $ runTemplate ctx (Template.Mobile.table files)
    pure $ Template.Mobile.view table
