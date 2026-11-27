-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the toml parser of configuration file. Config generated
-- from the config file will be combined with options to create the final `Config`.
-- Note, there are some configurations only available in the config file. e.g
-- OIDC doesn't have an cli option.
module Filehub.Config.Toml (parseConfigFile, serializeConfig) where

import Filehub.Prelude
import Filehub.Auth.Types.OIDC qualified as Auth.OIDC
import Filehub.Auth.Types.Simple qualified as Auth.Simple
import Filehub.Config
import Filehub.Locale (Locale(..))
import Filehub.Plugin.Toml qualified as Plugin.Toml
import Filehub.Plugin.Types (StaticPlugins(..))
import Filehub.Theme (Theme (..), CustomTheme(..))
import Data.Map.Strict qualified as Map
import Network.URI qualified as URI
import System.FilePath.Extended (expandVars)
import Target.File qualified
import Target.S3 qualified
import Text.Toml (TomlTable, TomlValue(..), TomlError(..))
import Text.Toml qualified as Toml
import Log (LogLevel (..))


------------------------------------------------------------
-- Public API

serializeConfig :: Config Maybe -> Text
serializeConfig = Toml.encode . configToTable


parseConfigFile :: Maybe FilePath -> IO (Config Maybe)
parseConfigFile Nothing = pure mempty
parseConfigFile (Just filePath) = do
  tbl <- Toml.parseFile filePath >>= either (throwIO . userError . show) pure
  cfg <- either (throwIO . userError . show) pure (decodeConfig tbl)
  expandFSTargetEnvVars cfg
  where
    expandFSTargetEnvVars c = do
      let expand (FSTargetConfig t) = do
            root' <- expandVars t.root
            pure $ FSTargetConfig (t { Target.File.root = root' })
          expand x = pure x
      targets' <- traverse expand (coerce @_ @[TargetConfig] c.targets)
      pure $ c { targets = coerce @_ @Targets targets' }


------------------------------------------------------------
-- Decode

decodeConfig :: TomlTable -> Either TomlError (Config Maybe)
decodeConfig tbl = do
  port                  <- Toml.getOptInt  "port"       tbl
  theme_                <- Toml.getOptText "theme"      tbl >>= traverse parseTheme
  verbosity_            <- Toml.getOptText "verbosity"  tbl >>= traverse parseVerbosity
  readOnly              <- Toml.getOptBool "readonly"   tbl
  locale_               <- Toml.getOptText "locale"     tbl >>= traverse parseLocale
  customThemeDark       <- fmap (fmap (Just . CustomThemeDark)) (decodeOptTheme "dark-theme"  tbl)
  customThemeLight      <- fmap (fmap (Just . CustomThemeLight)) (decodeOptTheme "light-theme" tbl)
  targetTbls            <- Toml.getArrayOfTables "target" tbl
  loginTbls             <- Toml.getArrayOfTables "login"  tbl
  oidcTbls              <- Toml.getArrayOfTables "oidc"   tbl
  pluginTbls            <- Toml.getArrayOfTables "plugin" tbl
  pluginJsFiles         <- Toml.getStringArray   "plugin_js" tbl
  targets               <- Targets               <$> traverse decodeTarget  targetTbls
  simpleAuthUserRecords <- SimpleAuthUserRecords <$> traverse decodeLogin   loginTbls
  oidcAuthProviders     <- OidcAuthProviders     <$> traverse decodeOIDC    oidcTbls
  plugins               <- StaticPlugins         <$> traverse Plugin.Toml.decodePlugin pluginTbls
  pure Config
    { port                  = port
    , theme                 = theme_
    , verbosity             = verbosity_
    , readOnly              = readOnly
    , locale                = locale_
    , customThemeDark       = customThemeDark
    , customThemeLight      = customThemeLight
    , targets               = targets
    , simpleAuthUserRecords = simpleAuthUserRecords
    , oidcAuthProviders     = oidcAuthProviders
    , plugins               = plugins
    , pluginJsFiles         = pluginJsFiles
    }


decodeTarget :: TomlTable -> Either TomlError TargetConfig
decodeTarget tbl = do
  typ <- Toml.getText "type" tbl
  case typ of
    "fs" -> FSTargetConfig . Target.File.Config <$> Toml.getString "root"   tbl
    "s3" -> S3TargetConfig . Target.S3.Config   <$> Toml.getString "bucket" tbl
    _    -> Left (TomlError ("unknown target type: " <> typ))


decodeLogin :: TomlTable -> Either TomlError Auth.Simple.UserRecord
decodeLogin tbl = Auth.Simple.UserRecord
  <$> Toml.getString "username" tbl
  <*> Toml.getString "password" tbl


decodeOIDC :: TomlTable -> Either TomlError Auth.OIDC.Provider
decodeOIDC tbl = do
  name         <- Toml.getText      "name"          tbl
  issuerStr    <- Toml.getText      "issuer"        tbl
  clientId     <- Toml.getText      "client_id"     tbl
  clientSecret <- Toml.getText      "client_secret" tbl
  grantType    <- Toml.getText      "grant_type"    tbl
  allowedUsers <- Toml.getTextArray "allowed_users" tbl
  redirectStr  <- Toml.getText      "redirect_uri"  tbl
  issuer      <- maybe (Left (TomlError "invalid issuer URI"))
                       Right (URI.parseURI (txt_unpack issuerStr))
  redirectURI <- maybe (Left (TomlError "invalid redirect_uri"))
                       Right (URI.parseURI (txt_unpack redirectStr))
  pure Auth.OIDC.Provider
    { name         = name
    , issuer       = issuer
    , clientId     = clientId
    , clientSecret = clientSecret
    , grantType    = grantType
    , allowedUsers = allowedUsers
    , redirectURI  = redirectURI
    }


decodeCustomTheme :: TomlTable -> Either TomlError CustomTheme
decodeCustomTheme tbl = CustomTheme
  <$> getHex "frontground"
  <*> getHex "background1"
  <*> getHex "background2"
  <*> getHex "background3"
  <*> getHex "primary"
  <*> getHex "secondary"
  <*> getHex "tertiary"
  <*> getHex "dark"
  <*> getHex "light"
  where
    getHex k = do
      t <- Toml.getText k tbl
      if "#" `txt_isPrefixOf` t && txt_length t == 7
        then Right t
        else Left (TomlError ("invalid color hex: " <> t))


decodeOptTheme :: Text -> TomlTable -> Either TomlError (Maybe CustomTheme)
decodeOptTheme key tbl =
  Toml.getOptTable key tbl >>= traverse decodeCustomTheme

------------------------------------------------------------
-- Encode

configToTable :: Config Maybe -> TomlTable
configToTable cfg = Map.fromList $ concat
  [ optKV "port"      TInt                        cfg.port
  , optKV "theme"     (TString . encodeTheme)     cfg.theme
  , optKV "verbosity" (TString . encodeVerbosity) cfg.verbosity
  , optKV "readonly"  TBool                       cfg.readOnly
  , optKV "locale"    (TString . encodeLocale)    cfg.locale
  , aoTs  "target"    (map targetToTable  (coerce cfg.targets))
  , aoTs  "login"     (map loginToTable   (coerce cfg.simpleAuthUserRecords))
  , aoTs  "oidc"      (map oidcToTable    (coerce cfg.oidcAuthProviders))
  , aoTs  "plugin"    (map Plugin.Toml.pluginToTable (coerce cfg.plugins))
  , pluginJsKV cfg.pluginJsFiles
  , darkThemeEntry
  , lightThemeEntry
  ]
  where
    optKV _ _ Nothing  = []
    optKV k f (Just v) = [(k, f v)]

    aoTs _ []  = []
    aoTs k ts  = [(k, TArrayOfTables ts)]

    pluginJsKV []  = []
    pluginJsKV fs  = [("plugin_js", TArray (map (TString . txt_pack) fs))]

    darkThemeEntry = case join cfg.customThemeDark of
      Nothing -> []
      Just ct -> [("dark-theme",  TTable (themeToTable (coerce ct)))]

    lightThemeEntry = case join cfg.customThemeLight of
      Nothing -> []
      Just ct -> [("light-theme", TTable (themeToTable (coerce ct)))]


targetToTable :: TargetConfig -> TomlTable
targetToTable (FSTargetConfig t) = Map.fromList
  [ ("type", TString "fs")
  , ("root", TString (txt_pack t.root))
  ]
targetToTable (S3TargetConfig t) = Map.fromList
  [ ("type",   TString "s3")
  , ("bucket", TString (txt_pack t.bucket))
  ]


loginToTable :: Auth.Simple.UserRecord -> TomlTable
loginToTable r = Map.fromList
  [ ("username", TString (txt_pack r.username))
  , ("password", TString (txt_pack r.password))
  ]


oidcToTable :: Auth.OIDC.Provider -> TomlTable
oidcToTable p = Map.fromList
  [ ("name",          TString p.name)
  , ("issuer",        TString (txt_pack (URI.uriToString id p.issuer "")))
  , ("client_id",     TString p.clientId)
  , ("client_secret", TString p.clientSecret)
  , ("grant_type",    TString p.grantType)
  , ("allowed_users", TArray  (map TString p.allowedUsers))
  , ("redirect_uri",  TString (txt_pack (URI.uriToString id p.redirectURI "")))
  ]


themeToTable :: CustomTheme -> TomlTable
themeToTable ct = Map.fromList
  [ ("frontground", TString ct.frontground)
  , ("background1", TString ct.background1)
  , ("background2", TString ct.background2)
  , ("background3", TString ct.background3)
  , ("primary",     TString ct.primary)
  , ("secondary",   TString ct.secondary)
  , ("tertiary",    TString ct.tertiary)
  , ("dark",        TString ct.dark)
  , ("light",       TString ct.light)
  ]


------------------------------------------------------------
-- Helpers

parseTheme :: Text -> Either TomlError Theme
parseTheme "dark"  = Right Dark
parseTheme "light" = Right Light
parseTheme t       = Left (TomlError ("unknown theme: " <> t))

encodeTheme :: Theme -> Text
encodeTheme Dark  = "dark"
encodeTheme Light = "light"


parseVerbosity :: Text -> Either TomlError LogLevel
parseVerbosity "info"      = Right LogInfo
parseVerbosity "attention" = Right LogAttention
parseVerbosity "trace"     = Right LogTrace
parseVerbosity t           = Left (TomlError ("unknown verbosity: " <> t))

encodeVerbosity :: LogLevel -> Text
encodeVerbosity LogInfo      = "info"
encodeVerbosity LogAttention = "attention"
encodeVerbosity LogTrace     = "trace"


parseLocale :: Text -> Either TomlError Locale
parseLocale "en"    = Right EN
parseLocale "zh_cn" = Right ZH_CN
parseLocale "zh_tw" = Right ZH_TW
parseLocale "zh_hk" = Right ZH_HK
parseLocale "ja"    = Right JA
parseLocale "es"    = Right ES
parseLocale "fr"    = Right FR
parseLocale "de"    = Right DE
parseLocale "ko"    = Right KO
parseLocale "ru"    = Right RU
parseLocale "pt"    = Right PT
parseLocale "it"    = Right IT
parseLocale t       = Left (TomlError ("unknown locale: " <> t))

encodeLocale :: Locale -> Text
encodeLocale EN    = "en"
encodeLocale ZH_CN = "zh_cn"
encodeLocale ZH_TW = "zh_tw"
encodeLocale ZH_HK = "zh_hk"
encodeLocale JA    = "ja"
encodeLocale ES    = "es"
encodeLocale FR    = "fr"
encodeLocale DE    = "de"
encodeLocale KO    = "ko"
encodeLocale RU    = "ru"
encodeLocale PT    = "pt"
encodeLocale IT    = "it"
