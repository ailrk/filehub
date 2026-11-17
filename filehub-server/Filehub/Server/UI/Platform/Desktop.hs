{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.UI.Platform.Desktop
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
import Data.Text (Text)
import Data.Text qualified as T
import Filehub.Locale (Phrase(..), phrase)
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
  let i18nScript = script_ [type_ "text/javascript"] (i18nPayload ctx.readOnly (phrase ctx.locale))
  pure $ runTemplate ctx (Template.Desktop.index i18nScript sideBar' view' toolBar')


i18nPayload :: Bool -> Phrase -> Text
i18nPayload readOnly p = "window.filehubI18n=" <> obj <> ";"
  where
    obj = "{" <> T.intercalate ","
      [ kv "delete"              p.contextmenu_delete
      , kv "rename"              p.contextmenu_rename
      , kv "copy"                p.contextmenu_copy
      , kv "download"            p.contextmenu_download
      , kv "details"             p.contextmenu_details
      , kv "edit"                p.contextmenu_edit
      , kv "view"                p.contextmenu_view
      , kv "play"                p.contextmenu_play
      , kv "open"                p.contextmenu_open
      , kv "cancel"              p.contextmenu_cancel
      , kv "selected"            p.contextmenu_selected
      , kv "confirmDelete1"      p.confirm_delete1
      , kv "confirmDeleteLocal"  p.confirm_delete_local
      , "\"readOnly\":" <> if readOnly then "true" else "false"
      ] <> "}"
    kv key val = "\"" <> key <> "\":\"" <> escape val <> "\""
    escape = T.replace "\\" "\\\\" . T.replace "\"" "\\\"" . T.replace "\n" "\\n" . T.replace "\r" "\\r" . T.replace "\t" "\\t"


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
