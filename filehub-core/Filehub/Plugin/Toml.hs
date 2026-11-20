module Filehub.Plugin.Toml
  ( serializePlugins
  , parsePlugins
  , pluginHint
  , pluginsCodec
  ) where

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
  , "# [[plugin.viewer]]"
  , "# mime  = \"text/plain\""
  , "# url   = \"https://example.com?file={file_path}\""
  , "#"
  , "# [[plugin.toolbar]]"
  , "# mime  = \"image\""
  , "# label = \"Edit in Photopea\""
  , "# url   = \"https://www.photopea.com#src={share_url}\""
  , "#"
  , "# URL placeholders: {file_path}  {file_name}  {mime_type}  {share_url}"
  ]


urlTemplate :: Key -> TomlCodec UrlTemplate
urlTemplate key = Toml.dimap (.unUrlTemplate) UrlTemplate (Toml.text key)


contextMenuEntry :: TomlCodec ContextMenuEntry
contextMenuEntry =
  ContextMenuEntry
  <$> Toml.text        "mime"  .= (.mime)
  <*> Toml.text        "label" .= (.label)
  <*> urlTemplate      "url"   .= (.url)


viewerEntry :: TomlCodec ViewerEntry
viewerEntry =
  ViewerEntry
  <$> Toml.text   "mime" .= (.mime)
  <*> urlTemplate "url"  .= (.url)


toolbarEntry :: TomlCodec ToolbarEntry
toolbarEntry =
  ToolbarEntry
  <$> Toml.text   "mime"  .= (.mime)
  <*> Toml.text   "label" .= (.label)
  <*> urlTemplate "url"   .= (.url)


staticPlugin :: TomlCodec StaticPlugin
staticPlugin =
  StaticPlugin
  <$> Toml.text                    "name"         .= (.name)
  <*> Toml.list contextMenuEntry   "context_menu" .= (.contextMenu)
  <*> Toml.list viewerEntry        "viewer"       .= (.viewer)
  <*> Toml.list toolbarEntry       "toolbar"      .= (.toolbar)


pluginsCodec :: Key -> TomlCodec StaticPlugins
pluginsCodec key = Toml.dimap (.unStaticPlugins) StaticPlugins (Toml.list staticPlugin key)
