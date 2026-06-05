{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.UI.Desktop
  ( fileDetailModal
  , editorModal
  , contextMenu
  , index
  , sideBar
  , view
  , toolBar
  )
  where

import Data.ClientPath qualified as ClientPath
import Filehub.Session.Types (TargetSessionData(..), Selected (..))
import Filehub.Session qualified as Session
import Filehub.Sort (sortFiles)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Server.Util (withQueryParam)
import Filehub.Template (TemplateContext(..), runTemplate, makeTemplateContext)
import Filehub.Types ( SessionId(..), ClientPath)
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Filehub.Session (TargetView(..), getTargetViews, getCurrentTarget, makeStorageCurrentTarget)
import Data.Coerce (coerce)
import Filehub.Monad (Filehub)
import Control.Monad (join)
import Filehub.Session.Pool (withSession)
import Control.Monad.Reader (MonadReader(..))
import UnliftIO (readTVarIO)
import Data.Traversable (for)


fileDetailModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
fileDetailModal sessionId mClientPath = do
  ctx@TemplateContext{ root } <- makeTemplateContext sessionId
  storage    <- makeStorageCurrentTarget sessionId
  clientPath <- withQueryParam mClientPath
  file       <- storage.get (ClientPath.fromClientPath root clientPath)
  pure $ runTemplate ctx (Template.Desktop.fileDetailModal file)


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath = do
  ctx@TemplateContext{ root } <- makeTemplateContext sessionId
  storage      <- makeStorageCurrentTarget sessionId
  clientPath   <- withQueryParam mClientPath
  let p        =  ClientPath.fromClientPath root clientPath
  file         <- storage.get p
  content      <- storage.read file
  let filename =  coerce takeFileName p
  pure $ runTemplate ctx (Template.Desktop.editorModal (clientPath, filename) content)


contextMenu :: SessionId -> [ClientPath] -> Filehub (Html ())
contextMenu sessionId clientPaths = do
  storage <- makeStorageCurrentTarget sessionId
  ctx@TemplateContext { root } <- makeTemplateContext sessionId
  case clientPaths of
    [clientPath] -> do
      file <- storage.get (ClientPath.fromClientPath root clientPath)
      pure $ runTemplate ctx (Template.Desktop.contextMenu1 file)
    _ -> do
      pure $ runTemplate ctx (Template.Desktop.contextMenuMany clientPaths)


index :: SessionId -> Filehub (Html ())
index sessionId = do
  ctx      <- makeTemplateContext sessionId
  sideBar' <- sideBar sessionId
  view'    <- view sessionId
  toolBar' <- toolBar sessionId
  pure $ runTemplate ctx (Template.Desktop.index sideBar' view' toolBar')


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  env <- ask
  join $ withSession sessionId \s -> do
    targetViews   <- getTargetViews env s
    currentTarget <- getCurrentTarget env s
    pure do
      ctx <- makeTemplateContext sessionId
      targets'    <- for targetViews \(TargetView target td) -> do
        selected' <- readTVarIO td.selected
        case selected' of
          Selected _ sels -> pure (target, length sels + 1)
          NoSelection     -> pure (target, 0)

      pure $ runTemplate ctx (Template.Desktop.sideBar targets' currentTarget)


view :: SessionId -> Filehub (Html ())
view sessionId = do
  ctx@TemplateContext { sortedBy = order } <- makeTemplateContext sessionId
  storage <- makeStorageCurrentTarget sessionId
  table <- do
    files <- sortFiles order <$> storage.lsCwd
    pure $ runTemplate ctx (Template.Desktop.table files)
  pure $ Template.Desktop.view table


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.toolBar
