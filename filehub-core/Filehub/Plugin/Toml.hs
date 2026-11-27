module Filehub.Plugin.Toml
  ( serializePlugins
  , parsePlugins
  , pluginHint
  , decodePlugin
  , pluginToTable
  ) where

import Filehub.Prelude
import Filehub.Plugin.Types
import Data.Map.Strict qualified as Map
import Text.Toml (TomlTable, TomlValue(..))
import Text.Toml qualified as Toml


serializePlugins :: [StaticPlugin] -> Text
serializePlugins [] = ""
serializePlugins ps = Toml.encode (Map.fromList [("plugin", TArrayOfTables (map pluginToTable ps))])


parsePlugins :: Text -> Either Text [StaticPlugin]
parsePlugins t
  | txt_null (txt_strip t) = Right []
  | otherwise = case Toml.parse t of
      Left (Toml.TomlError e) -> Left e
      Right root ->
        case Toml.getArrayOfTables "plugin" root of
          Left (Toml.TomlError e) -> Left e
          Right tbls ->
            case traverse decodePlugin tbls of
              Left (Toml.TomlError e) -> Left e
              Right ps               -> Right ps


pluginHint :: Text
pluginHint = txt_unlines
  [ "# Define plugins in TOML. Each plugin is a [[plugin]] table."
  , "#"
  , "# [[plugin]]"
  , "# name = \"My Plugin\""
  , "#"
  , "# [[plugin.context_menu]]"
  , "# mime  = \"image\""
  , "# label = \"Open in viewer\""
  , "# url   = \"https://example.com?path={file_path}\""
  , "#"
  , "# [[plugin.context_menu]]"
  , "# mime  = \"text\""
  , "# label = \"Copy path\""
  , "# script = \"\"\""
  , "#   function run({ filePath, fileName, mimeType }, ui) {"
  , "#     const win = ui.window({ title: fileName });"
  , "#     win.body.textContent = filePath;"
  , "#     win.show();"
  , "#   }"
  , "# \"\"\""
  , "#"
  , "# [[plugin.viewer]]"
  , "# mime  = \"text/plain\""
  , "# url   = \"https://example.com?file={file_path}\""
  , "#"
  , "# [[plugin.toolbar]]"
  , "# mime  = \"image\""
  , "# label = \"Edit in Photopea\""
  , "# url   = \"https://www.photopea.com#src={share_url}\""
  , "#"
  , "# URL placeholders:  {file_path}  {file_name}  {mime_type}  {share_url}"
  , "# Script: define function run({ filePath, fileName, mimeType }, ui) { ... }"
  , "#   ui.window({ title })    →  { body: HTMLElement, show(), close() }"
  , "#   ui.notify(msg, kind)    →  balloon ('info' or 'error')"
  , "#   ui.fetch(path, opts)    →  fetch() scoped to this server"
  , "#   ui.selectedFiles()      →  string[] of currently selected paths"
  , "#   ui.refresh()            →  reload the file list"
  ]


------------------------------------------------------------
-- Encode

pluginToTable :: StaticPlugin -> TomlTable
pluginToTable p = Map.fromList $
  [ ("name", TString p.name)
  , ("context_menu", TArrayOfTables (map contextMenuToTable p.contextMenu))
  , ("viewer",       TArrayOfTables (map viewerToTable p.viewer))
  , ("toolbar",      TArrayOfTables (map toolbarToTable p.toolbar))
  ]

contextMenuToTable :: ContextMenuEntry -> TomlTable
contextMenuToTable e = Map.fromList $
  [ ("mime",  TString e.mime)
  , ("label", TString e.label)
  ] ++ actionFields e.action

viewerToTable :: ViewerEntry -> TomlTable
viewerToTable e = Map.fromList $
  ("mime", TString e.mime) : actionFields e.action

toolbarToTable :: ToolbarEntry -> TomlTable
toolbarToTable e = Map.fromList $
  [ ("mime",  TString e.mime)
  , ("label", TString e.label)
  ] ++ actionFields e.action

actionFields :: EntryAction -> [(Text, TomlValue)]
actionFields (UrlAction (UrlTemplate u)) = [("url",    TString u)]
actionFields (ScriptAction s)            = [("script", TString s)]


------------------------------------------------------------
-- Decode

decodePlugin :: TomlTable -> Either Toml.TomlError StaticPlugin
decodePlugin tbl = do
  name        <- Toml.getText "name" tbl
  cmTbls      <- Toml.getArrayOfTables "context_menu" tbl
  viewerTbls  <- Toml.getArrayOfTables "viewer" tbl
  toolbarTbls <- Toml.getArrayOfTables "toolbar" tbl
  contextMenu <- traverse decodeContextMenu cmTbls
  viewer      <- traverse decodeViewer viewerTbls
  toolbar     <- traverse decodeToolbar toolbarTbls
  pure StaticPlugin { name = name, contextMenu = contextMenu, viewer = viewer, toolbar = toolbar }

decodeContextMenu :: TomlTable -> Either Toml.TomlError ContextMenuEntry
decodeContextMenu tbl = do
  mime   <- Toml.getText "mime"  tbl
  label  <- Toml.getText "label" tbl
  action <- decodeAction tbl
  pure ContextMenuEntry { mime = mime, label = label, action = action }

decodeViewer :: TomlTable -> Either Toml.TomlError ViewerEntry
decodeViewer tbl = do
  mime   <- Toml.getText "mime" tbl
  action <- decodeAction tbl
  pure ViewerEntry { mime = mime, action = action }

decodeToolbar :: TomlTable -> Either Toml.TomlError ToolbarEntry
decodeToolbar tbl = do
  mime   <- Toml.getText "mime"  tbl
  label  <- Toml.getText "label" tbl
  action <- decodeAction tbl
  pure ToolbarEntry { mime = mime, label = label, action = action }

decodeAction :: TomlTable -> Either Toml.TomlError EntryAction
decodeAction tbl =
  case (Toml.getOptText "url" tbl, Toml.getOptText "script" tbl) of
    (Right (Just u), _)         -> Right (UrlAction (UrlTemplate u))
    (_, Right (Just s))         -> Right (ScriptAction s)
    (Left e, _)                 -> Left e
    (_, Left e)                 -> Left e
    (Right Nothing, Right Nothing) ->
      Left (Toml.TomlError "entry must have url or script")
