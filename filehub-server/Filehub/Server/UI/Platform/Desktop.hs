{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.UI.Platform.Desktop
  ( editorModal
  , index
  , view
  , toolBar
  , pluginsDataEl
  )
  where

import Filehub.Prelude
import Data.ClientPath (AbsPath(..))
import Filehub.Locale (Phrase(..), phrase)
import Filehub.Monad (Filehub)
import Filehub.Plugin.Types (StaticPlugin(..), ContextMenuEntry(..), ToolbarEntry(..), UrlTemplate(..), EntryAction(..))
import Filehub.Session (makeStorageCurrentTarget, Storage(..), fromClientPathChecked)
import Filehub.Server.UI.Platform.Common (sideBar)
import Filehub.Sort (sortFiles)
import Filehub.Server.Util (withQueryParam)
import Filehub.Session.Types (TargetView(..))
import Filehub.Template (TemplateContext(..), runTemplate, makeTemplateContext)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Types ( SessionId(..), ClientPath)
import Lucid
import Prelude hiding (readFile)
import System.FilePath (takeFileName)
import Target.Home (Home)
import Target.Types (handleTarget, targetHandler, AnyTarget(..))


editorModal :: SessionId -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId mClientPath = do
  ctx@TemplateContext{ root } <- makeTemplateContext sessionId
  storage      <- makeStorageCurrentTarget sessionId
  clientPath   <- withQueryParam mClientPath
  p            <- fromClientPathChecked root clientPath
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
  let i18nScript = script_ [type_ "text/javascript"] $
        i18nPayload ctx.readOnly (phrase ctx.locale)
  pure $ runTemplate ctx (Template.Desktop.index (i18nScript <> pluginsDataEl ctx.plugins ctx.pluginJsFiles) sideBar' view' toolBar')


i18nPayload :: Bool -> Phrase -> Text
i18nPayload readOnly p = "window.filehubI18n=" <> obj <> ";"
  where
    obj = "{" <> txt_intercalate ","
      [ kv "delete"              p.contextmenu_delete
      , kv "rename"              p.contextmenu_rename
      , kv "copy"                p.contextmenu_copy
      , kv "cut"                 p.contextmenu_cut
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
    escape = txt_replace "\t" "\\t" . txt_replace "\r" "\\r" . txt_replace "\n" "\\n" . txt_replace "\"" "\\\"" . txt_replace "\\" "\\\\"


pluginsPayload :: [StaticPlugin] -> Text
pluginsPayload ps = "window.filehubPlugins=" <> arr <> ";"
  where
    arr = "[" <> txt_intercalate "," (map encodePlugin ps) <> "]"
    encodePlugin p =
      "{\"name\":" <> str p.name <>
      ",\"contextMenu\":[" <> txt_intercalate "," (map encodeContextMenu p.contextMenu) <> "]" <>
      ",\"toolbar\":[" <> txt_intercalate "," (map encodeToolbar p.toolbar) <> "]" <>
      "}"
    encodeToolbar (ToolbarEntry { mime, label, action }) =
      "{\"mime\":" <> str mime <>
      ",\"label\":" <> str label <>
      ",\"action\":" <> encodeAction action <>
      "}"
    encodeContextMenu e =
      "{\"mime\":" <> str e.mime <>
      ",\"label\":" <> str e.label <>
      ",\"action\":" <> encodeAction e.action <>
      "}"
    encodeAction (UrlAction (UrlTemplate u)) =
      "{\"type\":\"url\",\"value\":" <> str u <> "}"
    encodeAction (ScriptAction s) =
      "{\"type\":\"script\",\"value\":" <> str s <> "}"
    str t = "\"" <> escape t <> "\""
    escape = txt_replace "\t" "\\t" . txt_replace "\r" "\\r" . txt_replace "\n" "\\n" . txt_replace "\"" "\\\"" . txt_replace "\\" "\\\\"


pluginsDataEl :: [StaticPlugin] -> [Text] -> Html ()
pluginsDataEl ps jsFiles =
  div_ [id_ "filehub-plugins"] $ do
    script_ [type_ "text/javascript"]
      (pluginsPayload ps <> "\nwindow.filehub=window.filehub||{};window.filehub._jsPlugins=[];window.filehub.registerPlugin=function(d){this._jsPlugins.push(d);};")
    mapM_ (\src -> script_ [type_ "text/javascript"] ("(function(){\n" <> src <> "\n})();")) jsFiles


view :: SessionId -> Filehub (Html ())
view sessionId = do
  ctx@TemplateContext { sortedBy = order, currentTarget = TargetView (AnyTarget tgt) _ } <- makeTemplateContext sessionId
  case handleTarget (AnyTarget tgt) [ targetHandler @Home \_ -> homeCanvas ] of
    Just canvas -> pure $ Template.Desktop.view canvas
    Nothing -> do
      storage <- makeStorageCurrentTarget sessionId
      table <- do
        files <- sortFiles order <$> storage.lsCwd
        pure $ runTemplate ctx (Template.Desktop.table files)
      pure $ Template.Desktop.view table
  where
    homeCanvas :: Html ()
    homeCanvas = div_ [ id_ "home-canvas", class_ "home-canvas" ] mempty


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.toolBar
