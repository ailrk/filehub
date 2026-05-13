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
import Filehub.Session (SessionId)
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Types (ClientPath)
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Data.Coerce (coerce)
import Filehub.Session (SessionGet(..))
import Filehub.Monad (Filehub)
import Control.Monad.Reader (asks)
import UnliftIO.STM (readTVarIO)
import Filehub.Session.Selected (AllSelected(..))


index :: SessionId -> Filehub (Html ())
index sessionId = do
  ctx                 <- makeTemplateContext sessionId
  sideBar'            <- sideBar sessionId
  view'               <- view sessionId
  toolBar'            <- toolBar sessionId
  AllSelected {count} <- Selected.getAllSelected sessionId
  pure $ runTemplate ctx (Template.Mobile.index sideBar' toolBar' view' count)


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  currentTarget <- Session.get sessionId (.currentTarget)
  targets <- asks (.targets) >>= readTVarIO
  pure $ Template.Mobile.sideBar (fmap snd targets) currentTarget


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx (Template.Mobile.toolBar)


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath = do
  root         <- Session.get sessionId (.root)
  storage      <- Session.get sessionId (.storage)
  ctx          <- makeTemplateContext sessionId
  clientPath   <- withQueryParam mClientPath
  let p        =  ClientPath.fromClientPath root clientPath
  file         <- storage.get p
  content      <- storage.read file
  let filename = coerce takeFileName p
  pure $ runTemplate ctx (Template.Mobile.editorModal (clientPath, filename) content)


view :: SessionId -> Filehub (Html ())
view sessionId = do
  storage <- Session.get sessionId (.storage)
  ctx@TemplateContext{ sortedBy = order } <- makeTemplateContext sessionId
  table <- do
    files   <- sortFiles order <$> storage.lsCwd
    pure $ runTemplate ctx (Template.Mobile.table files)
  pure $ Template.Mobile.view table
