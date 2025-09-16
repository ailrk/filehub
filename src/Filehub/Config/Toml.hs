-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the toml parser of configuration file. Config generated
-- from the config file will be combined with options to create the final `Config`.
-- Note, there are some configurations only available in the config file. e.g
-- OIDC doesn't have an cli option.
module Filehub.Config.Toml where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful.Log (LogLevel (..))
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Auth.Simple qualified as Auth.Simple
import Filehub.Config
import Filehub.Locale (Locale(..))
import Filehub.Theme (Theme (..), CustomTheme(..))
import Network.URI (URI)
import Network.URI qualified as URI
import System.FilePath.Extended (expandVars)
import Target.File qualified
import Target.S3 qualified
import Toml (TomlCodec, (.=), Key)
import Toml qualified as Toml
import Toml.Codec (TomlBiMap)


simpleAuthUserRecord :: TomlCodec Auth.Simple.UserRecord
simpleAuthUserRecord =
  Auth.Simple.UserRecord
  <$> Toml.string "username" .= (.username)
  <*> Toml.string "password" .= (.password)


oidcAuthProvider :: TomlCodec Auth.OIDC.Provider
oidcAuthProvider =
  Auth.OIDC.Provider
  <$> Toml.text               "name"          .= (.name)
  <*> Toml.match _URI         "issuer"        .= (.issuer)
  <*> Toml.text               "client_id"     .= (.clientId)
  <*> Toml.text               "client_secret" .= (.clientSecret)
  <*> Toml.text               "grant_type"    .= (.grantType)
  <*> Toml.arrayOf Toml._Text "allowed_users" .= (.allowedUsers)
  <*> Toml.match _URI         "redirect_uri"  .= (.redirectURI)


targetConfig :: TomlCodec TargetConfig
targetConfig =
      Toml.dimatch match1 FSTargetConfig fs
  <|> Toml.dimatch match2 S3TargetConfig s3
  where
    match1 (FSTargetConfig t) = Just t; match1 _ = Nothing
    match2 (S3TargetConfig t) = Just t; match2 _ = Nothing
    fs = Target.File.Config <$> (Toml.validateIf (== "fs") Toml._Text "type" .= const "fs" *> Toml.string "root" .= (.root))
    s3 = Target.S3.Config <$> (Toml.validateIf (== "s3") Toml._Text "type" .= const "s3" *> Toml.string "bucket") .= (.bucket)


targetConfigs :: Key -> TomlCodec Targets
targetConfigs key = Toml.dimap (.unTargets) Targets (Toml.list targetConfig key)


simpleAuthUserRecords :: Key -> TomlCodec SimpleAuthUserRecords
simpleAuthUserRecords key = Toml.dimap (.unSimpleAuthUserRecords) SimpleAuthUserRecords (Toml.list simpleAuthUserRecord key)


oidcAuthProviders :: Key -> TomlCodec OidcAuthProviders
oidcAuthProviders key = Toml.dimap (.unOidcAuthProviders) OidcAuthProviders (Toml.list oidcAuthProvider key)



theme :: Key -> TomlCodec Theme
theme =
  Toml.textBy
    (\case
      Dark -> "dark"
      Light -> "light")
    (\case
      "dark" -> Right Dark
      "light" -> Right Light
      _ -> Left "unknown theme")


verbosity :: Key -> TomlCodec LogLevel
verbosity =
  Toml.textBy
    (\case
      LogInfo -> "info"
      LogAttention -> "attention"
      LogTrace -> "trace")
    (\case
      "info" -> Right LogInfo
      "attention" -> Right LogAttention
      "trace" -> Right LogTrace
      _ -> Left "unknown log level")


locale :: Key -> TomlCodec Locale
locale =
  Toml.textBy
    (\case
    EN      -> "en"
    ZH_CN   -> "zh_cn"
    ZH_TW   -> "zh_tw"
    ZH_HK   -> "zh_hk"
    JA      -> "ja"
    ES      -> "es"
    FR      -> "fr"
    DE      -> "de"
    KO      -> "ko"
    RU      -> "ru"
    PT      -> "pt"
    IT      -> "it")
    (\case
    "en"    -> Right EN;
    "zh_cn" -> Right ZH_CN
    "zh_tw" -> Right ZH_TW
    "zh_hk" -> Right ZH_HK
    "ja"    -> Right JA
    "es"    -> Right ES
    "fr"    -> Right FR
    "de"    -> Right DE
    "ko"    -> Right KO
    "ru"    -> Right RU
    "pt"    -> Right PT
    "it"    -> Right IT
    _       -> Left "unknown locale")


customTheme :: Key -> TomlCodec CustomTheme
customTheme key=
  Toml.table
    (CustomTheme
      <$> colorHex "frontground" .= (.frontground)
      <*> colorHex "background1" .= (.background1)
      <*> colorHex "background2" .= (.background2)
      <*> colorHex "background3" .= (.background3)
      <*> colorHex "primary"     .= (.primary)
      <*> colorHex "secondary"   .= (.secondary)
      <*> colorHex "tertiary"    .= (.tertiary)
      <*> colorHex "dark"        .= (.dark)
      <*> colorHex "light"       .= (.light))
    key


-- | If we don't wrap custom theme in newtype tomland will parse the table in order regardless
-- of the key for some reasons.
customThemeDark :: Key -> TomlCodec CustomThemeDark
customThemeDark key = Toml.dimap (.unCustomThemeDark) CustomThemeDark (customTheme key)


customThemeLight :: Key -> TomlCodec CustomThemeLight
customThemeLight key = Toml.dimap (.unCustomThemeLight) CustomThemeLight (customTheme key)


-- | Maybe maybe?
dioptional2 :: TomlCodec a -> TomlCodec (Maybe (Maybe a))
dioptional2 = Toml.dioptional . Toml.dioptional


config :: TomlCodec (Config Maybe)
config = Config
  <$> Toml.dioptional (Toml.int            "port")        .= (.port)
  <*> Toml.dioptional (theme               "theme")       .= (.theme)
  <*> Toml.dioptional (verbosity           "verbosity")   .= (.verbosity)
  <*> Toml.dioptional (Toml.bool           "readonly")    .= (.readOnly)
  <*> Toml.dioptional (locale              "locale")      .= (.locale)
  <*> dioptional2     (customThemeDark     "dark-theme")  .= (.customThemeDark)
  <*> dioptional2     (customThemeLight    "light-theme") .= (.customThemeLight)
  <*> (targetConfigs                       "target")      .= (.targets)
  <*> (simpleAuthUserRecords               "login")       .= (.simpleAuthUserRecords)
  <*> (oidcAuthProviders                   "oidc")        .= (.oidcAuthProviders)


-- | It should be called at the top level, let it throw if we failed to decode.
parseConfigFile :: Maybe FilePath -> IO (Config Maybe)
parseConfigFile (Just filePath) = do
  cfg <- either (throwIO . userError . show) pure <$> Toml.decodeFileEither config filePath
  cfg
    >>= expandFSTargetEnvVars
    >>= targetCheck
  where
    expandFSTargetEnvVars c = do
      let expand (FSTargetConfig t) = do
            root' <- expandVars t.root
            pure $ FSTargetConfig (t { Target.File.root = root' })
          expand x = pure x
      targets' <- traverse expand (coerce @_ @[TargetConfig] c.targets)
      pure $ c { targets = coerce @_ @Targets targets' }
    targetCheck c = do
      when (null (coerce @_ @[TargetConfig] c.targets)) do
        throwIO (userError "No target specified")
      pure c
parseConfigFile _ =
  pure (Config Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty mempty mempty)


------------------------------
-- Extra
------------------------------


_URI :: TomlBiMap URI Toml.AnyValue
_URI = uriText >>> Toml._Text
  where
    uriText :: TomlBiMap URI Text
    uriText = Toml.BiMap forward backward

    forward uri = Right . Text.pack . URI.uriToString id uri $ ""
    backward t = maybe
      (Left (Toml.ArbitraryError "invalid uri"))
      Right
      (URI.parseURI (Text.unpack t))


colorHex :: Key -> TomlCodec Text
colorHex key = Toml.validateIf (\s -> "#" `Text.isPrefixOf` s && Text.length s == 7) Toml._Text key
