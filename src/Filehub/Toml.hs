module Filehub.Toml where


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
import Data.Maybe (fromMaybe)


loginUser :: TomlCodec LoginUser
loginUser =
  LoginUser
  <$> Toml.string "username" .= (.username)
  <*> Toml.string "password" .= (.password)


oidcConfig :: TomlCodec Auth.OIDC.Provider
oidcConfig =
  Auth.OIDC.Provider
  <$> Toml.text "name" .= (.name)
  <*> Toml.text "issuer" .= (.issuer)
  <*> Toml.text "client_id" .= (.clientId)
  <*> Toml.text "client_secret" .= (.clientSecret)
  <*> Toml.text "grant_types" .= (.grantType)
  <*> Toml.arrayOf Toml._Text "allowed_users" .= (.allowedUsers)
  <*> Toml.arrayOf Toml._Text "redirect_uris" .= (.redirectURIs)


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


config :: TomlCodec (Config Maybe)
config = Config
  <$> Toml.dioptional (Toml.int                "port")      .= (.port)
  <*> Toml.dioptional (theme                   "theme")     .= (.theme)
  <*> Toml.dioptional (verbosity               "verbosity") .= (.verbosity)
  <*> Toml.dioptional (Toml.bool               "readonly")  .= (.readOnly)
  <*> Toml.dioptional (Toml.list targetConfig  "target")    .= (.targets)
  <*> Toml.dioptional (Toml.list loginUser     "login")     .= (.loginUsers)


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
      targets' <- maybe (pure []) (traverse expand) c.targets
      pure $ c { targets = Just targets' }
    targetCheck c = do
      when (null $ fromMaybe [] c.targets) do
        throwIO (userError "No target specified")
      pure c
parseConfigFile _ = pure (Config Nothing Nothing Nothing Nothing Nothing Nothing)
