{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
--
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements a OIDC client for filehub.
module Filehub.Auth.OIDC where

import Data.Text (Text)
import Servant (QueryParam, QueryParam', Proxy (..), Get, NoContent, Post, ReqBody, FormUrlEncoded, JSON, Required, linkURI)
import Servant qualified
import Servant.HTML.Lucid (HTML)
import GHC.Generics (Generic)
import Servant.Client (ClientM, client)
import Web.FormUrlEncoded (ToForm)
import Data.Aeson (FromJSON)
import Servant.Links (safeLink)
import Network.URI (URI (..))
import Control.Monad (replicateM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable (find)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful ((:>), Eff, IOE)
import Effectful (MonadIO (..))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Reader.Dynamic (Reader)
import Effectful.Reader.Dynamic (asks)
import Filehub.Env
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Orphan ()
import Filehub.Session.Types.SessionId (SessionId)
import Lens.Micro.Platform ()
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.URI (URIAuth (..))
import Network.URI (relativeTo)
import Network.URI qualified as URI
import Prelude hiding (init, readFile)
import Servant.Client (BaseUrl (..), Scheme (..), runClientM, mkClientEnv)
import Servant.Conduit ()
import System.Random (randomRIO)
import Filehub.Session qualified as Session


data Token = Token
  { idToken      :: Text
  , accessToken  :: Text
  , refreshToken :: Text
  , expiresIn    :: Int
  , tokenType    :: Text
  }
  deriving (Show, Eq, Generic)
instance FromJSON Token


data TokenForm = TokenForm
  { grantType     :: Text
  , code          :: Text
  , redirectUri   :: Text
  , clientId      :: Text
  , clientSecret  :: Maybe Text
  , codeVerifier  :: Maybe Text
  }
  deriving (Generic, Show)
instance ToForm TokenForm


-- | A subset of the OIDC authorization endpoint query parameters.
-- https://openid.net/specs/openid-connect-core-1_0.html#AuthorizationEndpoint
data Authorization = Authorization
  { responseType        :: Text
  , clientId            :: Text
  , redirectUri         :: Text
  , scope               :: Text
  , state               :: Text
  , codeChallenge       :: Maybe Text
  , codeChallengeMethod :: Maybe Text
  }


-- | The servant type of the Authorization endpoint query parameter
type AuthorizationApi
  =          QueryParam' '[Required] "response_type" Text
  Servant.:> QueryParam' '[Required] "client_id" Text
  Servant.:> QueryParam' '[Required] "redirect_uri" Text
  Servant.:> QueryParam' '[Required] "scope" Text
  Servant.:> QueryParam' '[Required] "state" Text
  Servant.:> QueryParam              "code_challenge" Text
  Servant.:> QueryParam              "code_challenge_method" Text
  Servant.:> Get '[HTML] Servant.NoContent


data User = User
  { sub   :: Text
  , user  :: Text
  , email :: Maybe Text
  , token :: Token
  }
  deriving (Show, Eq)


-- | OIDC Provider
data Provider = Provider
  { name         :: Text
  , issuer       :: URI
  , clientId     :: Text
  , clientSecret :: Text
  , grantType    :: Text
  , allowedUsers :: [Text]
  , redirectURI  :: URI
  }
  deriving (Show, Eq)


newtype OIDCAuthProviders = OIDCAuthProviders [Provider]
  deriving (Show, Eq)



-- | contains Authorization parameters
authorizeLink :: Authorization -> URI
authorizeLink (Authorization
  { responseType
  , clientId
  , redirectUri
  , scope
  , state
  , codeChallenge
  , codeChallengeMethod
  })
    = linkURI
    $ authorizeLink'
        responseType
        clientId
        redirectUri
        scope
        state
        codeChallenge
  codeChallengeMethod
  where
    authorizeLink' =
      safeLink
        (Servant.Proxy @AuthorizationApi)
        (Servant.Proxy @AuthorizationApi)


type ExchangeTokenApi = Servant.ReqBody '[Servant.FormUrlEncoded] TokenForm Servant.:> Servant.Post '[Servant.JSON] Token



exchangeToken :: TokenForm -> ClientM Token
exchangeToken = client (Servant.Proxy @ExchangeTokenApi)



-- | Servant API type
type WellKnownAPI =
  ".well-known"
    Servant.:> "openid-configuration"
    Servant.:> Get '[JSON] WellKnownConfig


-- | Return value of Wellknown URI registry
-- https://openid.net/specs/openid-connect-discovery-1_0.html#IANA
data WellKnownConfig = WellKnownConfig
  { issuer                 :: Text
  , authorization_endpoint :: Text
  , token_endpoint         :: Text
  , jwks_uri               :: Text
  , response_types_supported :: [Text]
  , subject_types_supported  :: [Text]
  , id_token_signing_alg_values_supported :: [Text]
  , userinfo_endpoint      :: Maybe Text
  , end_session_endpoint   :: Maybe Text
  } deriving (Show, Generic)


instance FromJSON WellKnownConfig



loginAuthOIDCRedirectURI :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Text -> Eff es URI
loginAuthOIDCRedirectURI sessionId providerName = do
  OIDCAuthProviders providers <- asks @Env (.oidcAuthProviders)
  provider <- maybe
    (throwError (FilehubError InternalError "Invalid provider"))
    pure
    (find ((providerName ==) . (.name)) providers)
  state <- Text.pack <$> replicateM 32 (randomRIO ('a', 'z'))
  Session.setOIDCState sessionId (Just state)
  codeVerifier <- Text.pack <$> replicateM 43 (randomRIO ('0', 'z'))
  let codeChallenge
        = Text.decodeUtf8
        $ Base64.encode
        $ SHA256.finalize
        $ SHA256.update SHA256.init
        $ Text.encodeUtf8
        $ codeVerifier
  pure $ flip relativeTo provider.issuer
       $ authorizeLink
          Authorization
            { responseType         = "code"
            , clientId             = provider.clientId
            , redirectUri          = Text.pack $ URI.uriToString id provider.redirectURI ""
            , scope                = "openid profile email"
            , state                = state
            , codeChallenge        = Just codeChallenge
            , codeChallengeMethod  = Just "S256"
            }



wellKnownConfigClient :: ClientM WellKnownConfig
wellKnownConfigClient = client (Proxy :: Proxy WellKnownAPI)


-- | Query the standard /.well-known/openid-configuration endpoint from IdP.
wellknownOpenIdConfigration :: (IOE :> es, Error FilehubError :> es) => Provider -> Eff es WellKnownConfig
wellknownOpenIdConfigration (Provider { issuer }) = do
  manager <- liftIO $ newManager defaultManagerSettings
  baseUri <- either (\err -> throwError (FilehubError InternalError $ Text.unpack err)) pure (uriToBaseUrl issuer)

  eWellKnownConfig <- liftIO $ runClientM wellKnownConfigClient (mkClientEnv manager baseUri)
  case eWellKnownConfig of
    Right config -> pure config
    Left err -> throwError (FilehubError InternalError (show err))
  where
    uriToBaseUrl :: URI -> Either Text BaseUrl
    uriToBaseUrl uri = do
        auth <- maybe (Left "URI has no authority") Right (uriAuthority uri)
        scheme <- case uriScheme uri of
                       "https:" -> pure Https
                       "http:"  -> pure Http
                       _        -> Left "Unsupported scheme"
        let port = if null (uriPort auth)
                     then if scheme == Https then 443 else 80
                     else read (tail $ uriPort auth) -- drop leading ':'
            path = uriPath uri
        pure $ BaseUrl scheme (uriRegName auth) port path
