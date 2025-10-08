{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements a OIDC client for filehub.
module Filehub.Auth.OIDC
  ( SomeOIDCFlow(..)
  , OIDCFlow(..)
  , Provider(..)
  , OIDCAuthProviders(..)
  , User(..)
  , Authorization(..)
  , AuthUrl(..)
  , init
  , authorize
  , callback
  , exchangeToken
  , verifyToken
  , authenticateSession
  , getSessionOIDCFlow
  , setSessionOIDCFlow
  )
  where

import Data.Text (Text)
import Servant (QueryParam, QueryParam', Proxy (..), Get, NoContent, Post, ReqBody, FormUrlEncoded, JSON, Required, linkURI)
import Servant qualified
import Servant.HTML.Lucid (HTML)
import GHC.Generics (Generic)
import Servant.Client (ClientM, client)
import Web.FormUrlEncoded (ToForm)
import Data.Aeson (FromJSON, Value, (.:))
import Servant.Links (safeLink)
import Network.URI (URI (..))
import Control.Monad (replicateM, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64.URL
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
import Lens.Micro.Platform ((^.), (.~))
import Network.URI (URIAuth (..))
import Network.URI (relativeTo)
import Network.URI qualified as URI
import Prelude hiding (init, readFile)
import Servant.Client (BaseUrl (..), Scheme (..), runClientM, mkClientEnv)
import Servant.Conduit ()
import System.Random (randomRIO)
import Data.Functor.Identity (Identity (..))
import Web.JWT (JWT, VerifiedJWT, JWTClaimsSet (..), JOSEHeader (..))
import Web.JWT qualified as JWT
import Data.Aeson.Types qualified as Aeson
import Crypto.PubKey.RSA qualified as RSA
import Crypto.Number.Serialize (os2ip)
import Data.Function ((&))
import Data.String.Interpolate (i)
import GHC.Records (HasField(..))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX qualified as Time
import Filehub.Auth.Types (AuthId, Auth (..), createAuthId)
import Filehub.Session.Types.SessionId (SessionId)
import Filehub.ActiveUser.Types (ActiveUser(..))
import Effectful.Log (Log)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session qualified as Session
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import UnliftIO (tryIO, Exception (..))


newtype OIDCState    = OIDCState Text
newtype CodeVerifier = CodeVerifier Text
newtype AuthUrl      = AuthUrl URI
newtype OIDCCode     = OIDCCode Text


data Inited
data AuthRequestPrepared
data CallbackCalled
data TokenExchanged
data TokenVerified
data SessionAuthenticated


-- | The OIDC stages
data OIDCFlow s where
  Inited               :: Provider -> OIDCFlow Inited
  AuthRequestPrepared  :: Provider -> WellKnownConfig Identity -> OIDCState -> CodeVerifier -> AuthUrl -> OIDCFlow AuthRequestPrepared
  CallbackCalled       :: Provider -> WellKnownConfig Identity -> CodeVerifier -> OIDCCode -> OIDCFlow CallbackCalled
  TokenExchanged       :: WellKnownConfig Identity -> TokenUnverified -> OIDCFlow TokenExchanged
  TokenVerified        :: Token -> OIDCFlow TokenVerified
  SessionAuthenticated :: OIDCFlow SessionAuthenticated


data SomeOIDCFlow = forall s . SomeOIDCFlow (OIDCFlow s)


-- | Unveried token from the IdP
data TokenUnverified = TokenUnverified
  { id_token      :: Text
  , access_token  :: Maybe Text
  , refresh_token :: Maybe Text
  , expires_in    :: Int
  , token_type    :: Text
  }
  deriving (Show, Eq, Generic)
instance FromJSON TokenUnverified


-- | Verified Token from the IdP
data Token = Token
  { idToken      :: JWT VerifiedJWT
  , accessToken  :: Maybe Text
  , refreshToken :: Maybe Text
  , expiresIn    :: Int
  , tokenType    :: Text
  }
  deriving (Show, Generic)


-- | User is a newtype wrapper over the token, which provies all necessary user information through
-- the JWT claim set. User has a set of virtual record fields that simplifies the access of the JWT claims
-- of `.idToken` of `Token`.
newtype User = User Token deriving (Show, Generic)

instance HasField "iss" User (Maybe Text) where
  getField (User (Token { idToken })) = JWT.stringOrURIToText <$> (JWT.claims idToken).iss

instance HasField "sub" User (Maybe Text) where
  getField (User (Token { idToken })) = JWT.stringOrURIToText <$> (JWT.claims idToken).sub

instance HasField "exp" User (Maybe UTCTime) where
  getField (User (Token { idToken })) = Time.posixSecondsToUTCTime . JWT.secondsSinceEpoch <$> (JWT.claims idToken).exp

instance HasField "iat" User (Maybe UTCTime) where
  getField (User (Token { idToken })) = Time.posixSecondsToUTCTime . JWT.secondsSinceEpoch <$> (JWT.claims idToken).iat

instance HasField "accessToken"  User (Maybe Text) where
  getField (User (Token { accessToken })) = accessToken

instance HasField "refreshToken" User (Maybe Text) where
  getField (User (Token { refreshToken })) = refreshToken


data TokenForm = TokenForm
  { grant_type     :: Text
  , code           :: Text
  , redirect_uri   :: Text
  , client_id      :: Text
  , client_secret  :: Text
  , code_verifier  :: Text
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
  , nonce               :: Text
  , codeChallenge       :: Maybe Text
  , codeChallengeMethod :: Maybe Text
  }


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


-- | Servant API type
type WellKnownAPI = ".well-known" Servant.:> "openid-configuration" Servant.:> Get '[JSON] (WellKnownConfig Maybe)


-- | Return value of Wellknown URI registry
-- https://openid.net/specs/openid-connect-discovery-1_0.html#IANA
data WellKnownConfig f = WellKnownConfig
  { issuer                                :: f URI
  , authorization_endpoint                :: f URI
  , token_endpoint                        :: f URI
  , jwks_uri                              :: f URI
  , response_types_supported              :: f [Text]
  , subject_types_supported               :: f [Text]
  , id_token_signing_alg_values_supported :: f [Text]
  , userinfo_endpoint                     :: (Maybe Text)
  , end_session_endpoint                  :: (Maybe Text)
  } deriving (Generic)
deriving instance Show (WellKnownConfig Maybe)
deriving instance Show (WellKnownConfig Identity)
instance FromJSON (WellKnownConfig Maybe)


verifyWellKnownConfig :: WellKnownConfig Maybe -> Maybe (WellKnownConfig Identity)
verifyWellKnownConfig WellKnownConfig
  { issuer                                = Just issuer
  , authorization_endpoint                = Just authorization_endpoint
  , token_endpoint                        = Just token_endpoint
  , jwks_uri                              = Just jwks_uri
  , response_types_supported              = Just response_types_supported
  , subject_types_supported               = Just subject_types_supported
  , id_token_signing_alg_values_supported = Just id_token_signing_alg_values_supported
  , userinfo_endpoint                     = userinfo_endpoint
  , end_session_endpoint                  = end_session_endpoint
  } =
  Just $ WellKnownConfig
    { issuer                                = Identity issuer
    , authorization_endpoint                = Identity authorization_endpoint
    , token_endpoint                        = Identity token_endpoint
    , jwks_uri                              = Identity jwks_uri
    , response_types_supported              = Identity response_types_supported
    , subject_types_supported               = Identity subject_types_supported
    , id_token_signing_alg_values_supported = Identity id_token_signing_alg_values_supported
    , userinfo_endpoint                     = userinfo_endpoint
    , end_session_endpoint                  = end_session_endpoint
    }
verifyWellKnownConfig _ = Nothing


init :: (Reader Env :> es, Error FilehubError :> es) => Text -> Eff es (OIDCFlow Inited)
init providerName = do
  OIDCAuthProviders providers <- asks @Env (.oidcAuthProviders)
  provider <- maybe
    (throwError (FilehubError InternalError "Invalid provider"))
    pure
    (find ((providerName ==) . (.name)) providers)
  pure $ Inited provider


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


-- | Create oidc authorization uri. The server needs to redirect to the uri to start the OIDC
-- authentication flow
authorize :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => OIDCFlow Inited -> Eff es (OIDCFlow AuthRequestPrepared)
authorize (Inited provider) = do
  state        <- Text.pack <$> replicateM 32 (randomRIO ('a', 'z'))
  codeVerifier <- Text.pack <$> replicateM 43 (randomRIO ('a', 'z'))
  nonce        <- Text.pack <$> replicateM 16 (randomRIO ('a', 'z'))

  -- PKCE (Proof Key for Code Exchange) codeChallenge = BASE64URL(SHA256(codeVerifier)). The codeChallenge is sent to the IdP in the
  -- authentication stage. Later in token exchange stage we send codeVerifier again, the IdP use them to prevent code interception.
  let codeChallenge
        = Text.decodeUtf8
        $ Base64.URL.encodeUnpadded
        $ SHA256.finalize
        $ SHA256.update SHA256.init
        $ Text.encodeUtf8
        $ codeVerifier
  wellknownConfig@WellKnownConfig { authorization_endpoint = Identity authorization_endpoint } <- getWellknownOpenIdConfigration provider
  let url = flip relativeTo authorization_endpoint
          $ authorizeLink
            Authorization
              { responseType        = "code"
              , clientId            = provider.clientId
              , redirectUri         = Text.pack (URI.uriToString id provider.redirectURI "")
              , scope               = "openid profile email"
              , state               = state
              , nonce               = nonce
              , codeChallenge       = Just codeChallenge
              , codeChallengeMethod = Just "S256"
              }
  pure $ AuthRequestPrepared provider wellknownConfig (OIDCState state) (CodeVerifier codeVerifier) (AuthUrl url)
  where
    authorizeLink :: Authorization -> URI
    authorizeLink Authorization
      { responseType, clientId, redirectUri, scope, state, codeChallenge, codeChallengeMethod
      } = linkURI (authorizeLink' responseType clientId redirectUri scope state codeChallenge codeChallengeMethod)
    authorizeLink' =
      safeLink
        (Proxy @AuthorizationApi)
        (Proxy @AuthorizationApi)


callback :: (Error FilehubError :> es) => OIDCFlow AuthRequestPrepared -> Text -> Text -> Eff es (OIDCFlow CallbackCalled)
callback (AuthRequestPrepared provider wellknownOpenIdConfigration (OIDCState state') codeVerifier _) code state = do
  when (state' /= state) do
    throwError (FilehubError LoginFailed "OIDC callback state mismatch")
  pure $ CallbackCalled provider wellknownOpenIdConfigration codeVerifier (OIDCCode code)


-- | The servant type for token exchange endpoint. We only support client_secret_post as the token_endpoint_auth_method.
type ExchangeTokenApi = Servant.ReqBody '[Servant.FormUrlEncoded] TokenForm Servant.:> Post '[Servant.JSON] TokenUnverified


exchangeToken :: (Reader Env :> es, Error FilehubError :> es, IOE :> es) => OIDCFlow CallbackCalled ->  Eff es (OIDCFlow TokenExchanged)
exchangeToken
  (CallbackCalled
    provider
    (wellknownConfig@WellKnownConfig { token_endpoint = Identity token_endpoint })
    (CodeVerifier codeVerifier) (OIDCCode code)) = do
  manager <- asks @Env (.httpManager)
  let form =
        TokenForm
          { grant_type    = "authorization_code"
          , code          = code
          , redirect_uri  = Text.pack (URI.uriToString id provider.redirectURI "")
          , client_id     = provider.clientId
          , client_secret = provider.clientSecret
          , code_verifier = codeVerifier
          }

  baseUri <- either (\err -> throwError (FilehubError InternalError (Text.unpack err))) pure (uriToBaseUrl token_endpoint)
  runClientM (exchangeTokenClient form) (mkClientEnv manager baseUri) & liftIO . tryIO
    >>= either (\(e :: IOError) -> throwError (FilehubError InternalError (displayException e))) pure
    >>= either (\err -> throwError (FilehubError InternalError (show err))) pure
    >>= pure . TokenExchanged wellknownConfig
  where
    exchangeTokenClient :: TokenForm -> ClientM TokenUnverified
    exchangeTokenClient = client (Proxy @ExchangeTokenApi)


-- | The JWK Sets from provider's jwks_uri
-- https://datatracker.ietf.org/doc/html/rfc7517
data JWK = JWK
  { use :: Text
  , kty :: Text
  , kid :: Text
  , alg :: Text
  , n   :: Text
  , e   :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON JWK


verifyToken :: (Reader Env :> es, Error FilehubError :> es, IOE :> es) => OIDCFlow TokenExchanged -> Eff es (OIDCFlow TokenVerified)
verifyToken
  (TokenExchanged
    (WellKnownConfig { jwks_uri = Identity jwks_uri })
    TokenUnverified { id_token, access_token, refresh_token, expires_in, token_type }) = do
  manager           <- asks @Env (.httpManager)
  idTokenUnverified <- JWT.decode id_token & maybe (throwError (FilehubError LoginFailed "invalid id token: not a JWT")) pure

  let JOSEHeader { kid } = JWT.header idTokenUnverified

  jwks <- do
    baseUri <- uriToBaseUrl jwks_uri & either (\err -> throwError (FilehubError InternalError (Text.unpack err))) pure
    value   <- do
      runClientM (client (Proxy @(Get '[JSON] Value))) (mkClientEnv manager baseUri) & liftIO . tryIO
        >>= either (\(e :: IOError) -> throwError (FilehubError LoginFailed (displayException e))) pure
        >>= either (\err -> throwError (FilehubError LoginFailed (show err))) pure

    -- .keys.JWT[]
    let parseJWTS :: Value -> Aeson.Parser [JWK]
        parseJWTS = Aeson.withObject "JWKS" \o -> o .: "keys"

    case Aeson.parseEither parseJWTS value of
      Left err -> throwError (FilehubError LoginFailed [i|failed to parse jwt. #{err}|])
      Right xs -> pure xs

  jwk <- find (\(JWK { kid = kid' }) -> Just kid' == kid ) jwks
    & maybe (throwError (FilehubError LoginFailed "no jwk corresponds to expected kid")) pure

  verifySigner <- either (\err -> throwError (FilehubError LoginFailed (show err))) pure do
    nBytes <- Base64.URL.decodeUnpadded . Text.encodeUtf8 $ jwk.n
    eBytes <- Base64.URL.decodeUnpadded . Text.encodeUtf8 $ jwk.e
    let n' = os2ip nBytes
    let e' = os2ip eBytes
    pure
      . JWT.VerifyRSAPublicKey
      $ RSA.PublicKey
        { public_size = (ByteString.length nBytes)
        , public_n    = n'
        , public_e    = e'
        }

  idTokenVerified <- do
    maybe (throwError (FilehubError LoginFailed "invalid id token: failed to verify")) pure
      (JWT.verify verifySigner idTokenUnverified)

  when (Text.toLower token_type /= "bearer") do
    throwError (FilehubError LoginFailed (Text.unpack ("invalid token type: " <> token_type)))

  pure
    $ TokenVerified Token
      { idToken      = idTokenVerified
      , accessToken  = access_token
      , refreshToken = refresh_token
      , expiresIn    = expires_in
      , tokenType    = token_type
      }


authenticateSession :: (Reader Env :> es, IOE :> es) => SessionId -> OIDCFlow TokenVerified -> Eff es (OIDCFlow SessionAuthenticated)
authenticateSession sessionId (TokenVerified token) = do
  let user = User token
  authId <- createAuthId
  Session.setAuthId sessionId (Just authId)
  activeUser <- createActiveUser authId sessionId user
  ActiveUser.Pool.add activeUser
  pure SessionAuthenticated


-- | Query the standard /.well-known/openid-configuration endpoint from IdP.
getWellknownOpenIdConfigration :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => Provider -> Eff es (WellKnownConfig Identity)
getWellknownOpenIdConfigration (Provider { issuer }) = do
  manager          <- asks @Env (.httpManager)
  baseUri          <- uriToBaseUrl issuer & either (\err -> throwError (FilehubError InternalError (Text.unpack err))) pure
  result           <- runClientM wellKnownConfigClient (mkClientEnv manager baseUri) & liftIO . tryIO
  eWellKnownConfig <- result & either (\(e :: IOError) -> throwError (FilehubError InternalError (displayException e))) pure
  case verifyWellKnownConfig <$> eWellKnownConfig of
    Right (Just config) -> pure config
    Right Nothing       -> throwError (FilehubError InternalError "Invalid .well-known/openid-configuration")
    Left err            -> throwError (FilehubError InternalError (show err))
  where
    wellKnownConfigClient :: ClientM (WellKnownConfig Maybe)
    wellKnownConfigClient = client (Proxy :: Proxy WellKnownAPI)


uriToBaseUrl :: URI -> Either Text BaseUrl
uriToBaseUrl uri = do
    auth <- maybe (Left "URI has no authority") Right (uriAuthority uri)
    scheme <- case uriScheme uri of
                   "https:" -> pure Https
                   "http:"  -> pure Http
                   _        -> Left "Unsupported scheme"
    let port = if null (uriPort auth)
                 then if scheme == Https then 443 else 80
                 else read (tail (uriPort auth)) -- drop leading ':'
        path = uriPath uri
    pure $ BaseUrl scheme (uriRegName auth) port path


createActiveUser :: (IOE :> es) => AuthId -> SessionId -> User -> Eff es ActiveUser
createActiveUser authId sessionId user = do
  now <- liftIO Time.getCurrentTime
  pure ActiveUser
    { authId   = authId
    , loginAt  = now
    , sessions = [sessionId]
    , auth     = OIDC user
    }


-- | Get the current oidc flow.
getSessionOIDCFlow :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  Log :> es) => SessionId -> Eff es (Maybe SomeOIDCFlow)
getSessionOIDCFlow sessionId = (^. #oidcFlow) <$> Session.Pool.get sessionId


-- | Set the current oidc flow.
setSessionOIDCFlow :: (Reader Env :> es, IOE :> es) => SessionId -> Maybe (OIDCFlow s) -> Eff es ()
setSessionOIDCFlow sessionId (Just flow) = do
  Session.Pool.update sessionId \s -> s & #oidcFlow .~ (Just (SomeOIDCFlow flow))
setSessionOIDCFlow sessionId Nothing = do
  Session.Pool.update sessionId \s -> s & #oidcFlow .~ Nothing
