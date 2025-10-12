{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Component.Desktop
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
import Filehub.Monad ( Filehub )
import Filehub.Session.Types (TargetSessionData(..))
import Filehub.Session qualified as Session
import Filehub.Sort (sortFiles)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Server.Internal (withQueryParam)
import Filehub.Template (TemplateContext(..), runTemplate, makeTemplateContext)
import Filehub.Types ( SessionId(..), ClientPath, Selected(..))
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Filehub.Session (TargetView(..))
import Effectful.Error.Dynamic (throwError)
import Filehub.Error (FilehubError(..), Error'(InvalidPath))


fileDetailModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
fileDetailModal sessionId mClientPath = do
  ctx@TemplateContext{ root } <- makeTemplateContext sessionId
  clientPath <- withQueryParam mClientPath
  storage    <- Session.getStorage sessionId
  mFile      <- storage.get (ClientPath.fromClientPath root clientPath)
  case mFile of
    Just file -> pure $ runTemplate ctx (Template.Desktop.fileDetailModal file)
    Nothing   -> throwError (FilehubError InvalidPath "can't get file details")


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath = do
  ctx@TemplateContext{ root } <- makeTemplateContext sessionId
  clientPath <- withQueryParam mClientPath
  storage    <- Session.getStorage sessionId
  let p = ClientPath.fromClientPath root clientPath
  mFile <- storage.get p
  case mFile of
    Just file -> do
      content <- storage.read file
      let filename = takeFileName p
      pure $ runTemplate ctx (Template.Desktop.editorModal filename content)
    Nothing -> do
      throwError (FilehubError InvalidPath "can't edit file")


contextMenu :: SessionId -> [ClientPath] -> Filehub (Html ())
contextMenu sessionId clientPaths = do
  ctx@TemplateContext { root } <- makeTemplateContext sessionId
  case clientPaths of
    [clientPath] -> do
      storage <- Session.getStorage sessionId
      mFile   <- storage.get (ClientPath.fromClientPath root clientPath)
      case mFile of
        Just file -> pure $ runTemplate ctx (Template.Desktop.contextMenu1 file)
        Nothing   -> throwError (FilehubError InvalidPath "can't get detail of the file")
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
  ctx      <- makeTemplateContext sessionId
  targetViews <- Session.getSessionTargetViews sessionId
  let targets' = flip fmap targetViews \(TargetView target targetData) -> do
        case targetData.selected of
          Selected _ sels -> (target, length sels + 1)
          NoSelection     -> (target, 0)

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
