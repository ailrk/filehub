-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the toml parser of configuration file. Config generated
-- from the config file will be combined with options to create the final `Config`.
-- Note, there are some configurations only available in the config file. e.g
-- OIDC doesn't have an cli option.
module Filehub.Config.Toml where

import Toml qualified as Toml
import Toml (TomlCodec, (.=), Key)
import Filehub.Config
import Filehub.Theme (Theme (..))
import Filehub.ExpandEnv (expandVars)
import Filehub.Auth.Simple (LoginUser(..))
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Effectful.Log (LogLevel (..))
import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (when)
import Filehub.Locale (Locale(..))
import Toml.Codec (TomlBiMap)
import Network.URI (URI)
import Data.Text qualified as Text
import Network.URI qualified as URI
import Control.Category ((>>>))
import Data.Text (Text)


simpleAuthLoginUser :: TomlCodec LoginUser
simpleAuthLoginUser =
  LoginUser
  <$> Toml.string "username" .= (.username)
  <*> Toml.string "password" .= (.password)


oidcAuthProvider :: TomlCodec Auth.OIDC.Provider
oidcAuthProvider =
  Auth.OIDC.Provider
  <$> Toml.text "name"                        .= (.name)
  <*> Toml.match _URI "issuer"                .= (.issuer)
  <*> Toml.text "client_id"                   .= (.clientId)
  <*> Toml.text "client_secret"               .= (.clientSecret)
  <*> Toml.text "grant_type"                  .= (.grantType)
  <*> Toml.arrayOf Toml._Text "allowed_users" .= (.allowedUsers)
  <*> Toml.match _URI "redirect_uri"          .= (.redirectURI)


targetConfig :: TomlCodec TargetConfig
targetConfig =
      Toml.dimatch match1 FSTargetConfig fs
  <|> Toml.dimatch match2 S3TargetConfig s3
  where
    match1 (FSTargetConfig t) = Just t; match1 _ = Nothing
    match2 (S3TargetConfig t) = Just t; match2 _ = Nothing
    fs = FSTargetConfig_ <$> (Toml.validateIf (== "fs") Toml._Text "type" .= const "fs" *> Toml.string "root" .= (.root))
    s3 = S3TargetConfig_ <$> (Toml.validateIf (== "s3") Toml._Text "type" .= const "s3" *> Toml.string "bucket") .= (.bucket)


theme :: Key -> TomlCodec Theme
theme key =
  Toml.textBy
    (\case
      Dark -> "dark"
      Light -> "light")
    (\case
      "dark" -> Right Dark
      "light" -> Right Light
      _ -> Left "unknown theme")
    key


verbosity :: Key -> TomlCodec LogLevel
verbosity key =
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
    key


locale :: Key -> TomlCodec Locale
locale key
  =   Toml.dimatch (const $ Just "en")    (const EN)    (Toml.text key)
  <|> Toml.dimatch (const $ Just "zh_cn") (const ZH_CN) (Toml.text key)
  <|> Toml.dimatch (const $ Just "zh_tw") (const ZH_TW) (Toml.text key)
  <|> Toml.dimatch (const $ Just "zh_hk") (const ZH_HK) (Toml.text key)
  <|> Toml.dimatch (const $ Just "ja")    (const JA)    (Toml.text key)
  <|> Toml.dimatch (const $ Just "es")    (const ES)    (Toml.text key)
  <|> Toml.dimatch (const $ Just "fr")    (const FR)    (Toml.text key)
  <|> Toml.dimatch (const $ Just "de")    (const DE)    (Toml.text key)
  <|> Toml.dimatch (const $ Just "ko")    (const KO)    (Toml.text key)
  <|> Toml.dimatch (const $ Just "ru")    (const RU)    (Toml.text key)
  <|> Toml.dimatch (const $ Just "pt")    (const PT)    (Toml.text key)
  <|> Toml.dimatch (const $ Just "it")    (const IT)    (Toml.text key)


config :: TomlCodec (Config Maybe)
config = Config
  <$> Toml.dioptional (Toml.int     "port")      .= (.port)
  <*> Toml.dioptional (theme        "theme")     .= (.theme)
  <*> Toml.dioptional (verbosity    "verbosity") .= (.verbosity)
  <*> Toml.dioptional (Toml.bool    "readonly")  .= (.readOnly)
  <*> Toml.dioptional (locale       "locale")    .= (.locale)
  <*> Toml.list targetConfig        "target"     .= (.targets)
  <*> Toml.list simpleAuthLoginUser "login"      .= (.simpleAuthLoginUsers)
  <*> Toml.list oidcAuthProvider    "oidc"       .= (.oidcAuthProviders)


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
            pure $ FSTargetConfig (t { root = root' })
          expand x = pure x
      targets' <- traverse expand c.targets
      pure $ c { targets = targets' }
    targetCheck c = do
      when (null c.targets) do
        throwIO (userError "No target specified")
      pure c
parseConfigFile _ = pure (Config Nothing Nothing Nothing Nothing Nothing [] [] [])


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
      (Left $ Toml.ArbitraryError "invalid uri")
      Right
      (URI.parseURI (Text.unpack t))
