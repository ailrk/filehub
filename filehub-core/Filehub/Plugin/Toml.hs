module Filehub.Plugin.Toml
  ( serializePlugins
  , parsePlugins
  , pluginHint
  , pluginsCodec
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Text qualified as T
import Filehub.Plugin.Types
import Toml (TomlCodec, (.=), Key)
import Toml qualified


serializePlugins :: [StaticPlugin] -> Text
serializePlugins [] = ""
serializePlugins ps  = Toml.encode (pluginsCodec "plugin") (StaticPlugins ps)


parsePlugins :: Text -> Either Text [StaticPlugin]
parsePlugins t
  | T.null (T.strip t) = Right []
  | otherwise = case Toml.decode (pluginsCodec "plugin") t of
      Left errs -> Left (T.unlines (map (T.pack . show) errs))
      Right (StaticPlugins ps) -> Right ps


pluginHint :: Text
pluginHint = T.unlines
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
  , "#     navigator.clipboard.writeText(filePath);"
  , "#     ui.notify('Copied!');"
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
  , "#   ui.modal({ title })  →  { body, show(), close() }"
  , "#   ui.notify(msg, 'info'|'error')"
  ]


entryActionCodec :: TomlCodec EntryAction
entryActionCodec =
  Toml.dimatch matchUrl    (UrlAction . UrlTemplate) (Toml.text "url")
  <|>
  Toml.dimatch matchScript ScriptAction              (Toml.text "script")
  where
    matchUrl    (UrlAction (UrlTemplate u)) = Just u
    matchUrl    _                           = Nothing
    matchScript (ScriptAction s)            = Just s
    matchScript _                           = Nothing


contextMenuEntry :: TomlCodec ContextMenuEntry
contextMenuEntry =
  ContextMenuEntry
  <$> Toml.text        "mime"   .= (.mime)
  <*> Toml.text        "label"  .= (.label)
  <*> entryActionCodec          .= (.action)


viewerEntry :: TomlCodec ViewerEntry
viewerEntry =
  ViewerEntry
  <$> Toml.text   "mime"   .= (.mime)
  <*> entryActionCodec     .= (.action)


toolbarEntry :: TomlCodec ToolbarEntry
toolbarEntry =
  ToolbarEntry
  <$> Toml.text   "mime"   .= (.mime)
  <*> Toml.text   "label"  .= (.label)
  <*> entryActionCodec     .= (.action)


staticPlugin :: TomlCodec StaticPlugin
staticPlugin =
  StaticPlugin
  <$> Toml.text                    "name"         .= (.name)
  <*> Toml.list contextMenuEntry   "context_menu" .= (.contextMenu)
  <*> Toml.list viewerEntry        "viewer"       .= (.viewer)
  <*> Toml.list toolbarEntry       "toolbar"      .= (.toolbar)


pluginsCodec :: Key -> TomlCodec StaticPlugins
pluginsCodec key = Toml.dimap (.unStaticPlugins) StaticPlugins (Toml.list staticPlugin key)
