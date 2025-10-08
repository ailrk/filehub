{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Platform.Desktop where

import Control.Monad (forM)
import Data.ClientPath qualified as ClientPath
import Filehub.Env qualified as Env
import Filehub.Env (Env)
import Target.Types (Target(..))
import Filehub.Monad ( Filehub )
import Filehub.Session.Types (TargetSessionData(..))
import Filehub.Session qualified as Session
import Filehub.Sort (sortFiles)
import Filehub.Template.Platform.Desktop qualified as Template.Desktop
import Filehub.Template.Internal (runTemplate, TemplateContext(..))
import Filehub.Server.Internal (withQueryParam, makeTemplateContext)
import Filehub.Types ( SessionId(..), ClientPath, Selected(..))
import Target.Class (IsTarget(..))
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Effectful.Reader.Dynamic (asks)
import Filehub.Session (TargetView(..))


fileDetailModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
fileDetailModal sessionId mClientPath = do
  ctx@TemplateContext{ root } <- makeTemplateContext sessionId
  clientPath <- withQueryParam mClientPath
  storage    <- Session.getStorage sessionId
  file       <- storage.get (ClientPath.fromClientPath root clientPath)
  pure $ runTemplate ctx (Template.Desktop.fileDetailModal file)


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath = do
  ctx@TemplateContext{ root } <- makeTemplateContext sessionId
  clientPath <- withQueryParam mClientPath
  storage    <- Session.getStorage sessionId
  let p = ClientPath.fromClientPath root clientPath
  content <- do
    f <- storage.get p
    storage.read f
  let filename = takeFileName p
  pure $ runTemplate ctx (Template.Desktop.editorModal filename content)


contextMenu :: SessionId -> [ClientPath] -> Filehub (Html ())
contextMenu sessionId clientPaths = do
  ctx@TemplateContext { root } <- makeTemplateContext sessionId
  storage <- Session.getStorage sessionId
  files   <- traverse storage.get (ClientPath.fromClientPath root <$> clientPaths)
  pure $ runTemplate ctx (Template.Desktop.contextMenu files)


index :: SessionId -> Filehub (Html ())
index sessionId = do
  ctx      <- makeTemplateContext sessionId
  sideBar' <- sideBar sessionId
  view'    <- view sessionId
  toolBar' <- toolBar sessionId
  pure $ runTemplate ctx (Template.Desktop.index sideBar' view' toolBar')


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  ctx      <- makeTemplateContext sessionId
  targets  <- asks @Env (.targets)
  targets' <- forM targets \(Target backend) -> do
    let targetId = getTargetIdFromBackend backend
    Session.withTarget sessionId targetId \(TargetView target targetData _) _ -> do
      case targetData.selected of
        Selected _ sels -> pure (target, length sels + 1)
        NoSelection     -> pure (target, 0)
  currentTargetView <- Session.currentTarget sessionId
  pure $ runTemplate ctx (Template.Desktop.sideBar targets' currentTargetView)


view :: SessionId -> Filehub (Html ())
view sessionId = do
  ctx@TemplateContext { sortedBy = order } <- makeTemplateContext sessionId
  table <- do
    storage <- Session.getStorage sessionId
    files   <- sortFiles order <$> storage.lsCwd
    pure $ runTemplate ctx (Template.Desktop.table files)
  pure $ Template.Desktop.view table


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.toolBar
