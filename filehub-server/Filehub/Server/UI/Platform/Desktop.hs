{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.UI.Platform.Desktop
  ( editorModal
  , index
  , sideBar
  , view
  , toolBar
  )
  where

import Data.ClientPath qualified as ClientPath
import Data.ClientPath (AbsPath(..), Root(..))
import Data.Coerce (coerce)
import Data.DiskUsage (getDiskUsage, DiskUsage(..))
import Data.Text (Text)
import Data.Text qualified as T
import Filehub.Locale (Phrase(..), phrase)
import Filehub.Monad (Filehub)
import Filehub.Session (TargetView(..), getTargetViews, getCurrentTarget, makeStorageCurrentTarget, Storage(..))
import Filehub.Session.Pool (withSession)
import Filehub.Session.Types (TargetSessionData(..), Selected (..))
import Filehub.Sort (sortFiles)
import Filehub.Server.Util (withQueryParam)
import Filehub.Storage.S3 qualified as S3
import Filehub.Template (TemplateContext(..), runTemplate, makeTemplateContext)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Types ( SessionId(..), ClientPath)
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Control.Monad (join)
import Control.Monad.Reader (MonadReader(..))
import UnliftIO (readTVarIO, tryAny)
import Target.File (FileSys, Target(..))
import UnliftIO.Async (forConcurrently)
import Target.S3 (S3)
import Target.Types (handleTarget, targetHandler)


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
      , kv "modal_file"          p.modal_file
      , kv "modal_folder"        p.modal_folder
      , kv "modal_create"        p.modal_create
      , kv "modal_confirm"       p.modal_confirm
      , kv "modal_readonly"      p.modal_readonly
      , kv "modal_detail"        p.modal_detail
      , kv "detail_filename"     p.detail_filename
      , kv "detail_modified"     p.detail_modified
      , kv "detail_accessed"     p.detail_accessed
      , kv "detail_size"         p.detail_size
      , kv "detail_content_type" p.detail_content_type
      , kv "placeholder_newfile"  p.placeholder_newfile
      , kv "placeholder_newfoler" p.placeholder_newfoler
      , kv "control_panel_dark"  p.control_panel_dark
      , kv "control_panel_light" p.control_panel_light
      , kv "new_file"            p.control_panel_new_file
      , kv "new_folder"          p.control_panel_new_folder
      , kv "upload"              p.control_panel_upload
      , kv "paste"               p.control_panel_paste
      , kv "share"               p.contextmenu_share
      , kv "share_link_indefinite" p.share_link_indefinite
      , kv "share_link_singleuse"  p.share_link_singleuse
      , kv "share_link_expiry"     p.share_link_expiry
      , kv "share_passcode"        p.share_passcode
      , kv "share_create"          p.share_create
      , kv "modal_readonly"        p.modal_readonly
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
      targets'    <- forConcurrently targetViews \(TargetView target td) -> do
        selected' <- readTVarIO td.selected
        let selCount = case selected' of
              Selected _ sels -> length sels + 1
              NoSelection     -> 0
        mUsage <- case handleTarget target
          [ targetHandler @FileSys \(FileBackend { root = Root (AbsPath p) }) -> Left p
          , targetHandler @S3      \s3                                         -> Right s3
          ] of
            Just (Left fsPath) -> getDiskUsage fsPath
            Just (Right s3)    -> do
              result <- tryAny (S3.getBucketSize s3)
              pure $ case result of
                Right used -> Just DiskUsage { total = 0, used }
                Left _     -> Nothing
            Nothing            -> pure Nothing
        pure (target, selCount, mUsage)

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
