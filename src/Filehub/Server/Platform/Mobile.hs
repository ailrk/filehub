{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Platform.Mobile where

import Effectful.Reader.Dynamic (asks)
import Filehub.ClientPath qualified as ClientPath
import Filehub.Env (Env)
import Filehub.Env qualified as Env
import Filehub.Error ( withServerError, withServerError )
import Filehub.Monad ( Filehub )
import Filehub.Server.Internal (withQueryParam, makeTemplateContext)
import Filehub.Session (SessionId)
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Storage (getStorage, Storage(..))
import Filehub.Template.Internal (runTemplate, TemplateContext(..))
import Filehub.Template.Platform.Mobile qualified as Template.Mobile
import Filehub.Types (ClientPath)
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)


index :: SessionId -> Filehub (Html ())
index sessionId = do
  ctx           <- makeTemplateContext sessionId
  sideBar'      <- sideBar sessionId
  view'         <- view sessionId
  selectedCount <- Selected.countSelected sessionId & withServerError
  toolBar'      <- toolBar sessionId
  pure $ runTemplate ctx (Template.Mobile.index sideBar' toolBar' view' selectedCount)


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = withServerError $
  Template.Mobile.sideBar
  <$> asks @Env (.targets)
  <*> Session.currentTarget sessionId


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx (Template.Mobile.toolBar)


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath = withServerError do
  clientPath <- withQueryParam mClientPath
  storage    <- getStorage sessionId
  root       <- Session.getRoot sessionId
  let p      =  ClientPath.fromClientPath root clientPath
  content <- do
    f <- storage.get p
    storage.read f
  let filename = takeFileName p
  readOnly <- asks @Env (.readOnly)
  pure $ Template.Mobile.editorModal readOnly filename content


view :: SessionId -> Filehub (Html ())
view sessionId = do
  ctx@TemplateContext{ sortedBy = order } <- makeTemplateContext sessionId
  table <- withServerError do
    storage <- getStorage sessionId
    files   <- sortFiles order <$> storage.lsCwd
    pure $ runTemplate ctx (Template.Mobile.table files)
  pure $ Template.Mobile.view table
