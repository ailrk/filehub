{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Auth.Types.OIDC
  ( Provider(..)
  , OIDCAuthProviders(..)
  , TokenUnverified(..)
  , Token(..)
  , User(..)
  , Authorization(..)
  , WellKnownConfig(..)
  , OIDCState(..)
  , CodeVerifier(..)
  , AuthUrl(..)
  , OIDCCode(..)
  , OIDCFlow(..)
  , SomeOIDCFlow(..)
  , Inited
  , AuthRequestPrepared
  , CallbackCalled
  , TokenExchanged
  , TokenVerified
  , SessionAuthenticated
  )
  where


import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX qualified as Time
import Filehub.Orphan ()
import GHC.Generics (Generic)
import GHC.Records (HasField(..))
import Network.URI (URI(..))
import Prelude hiding (init, readFile)
import Servant.Conduit ()
import Web.JWT (JWT, VerifiedJWT, JWTClaimsSet (..))
import Web.JWT qualified as JWT
import Text.Debug (Debug)
import Data.Functor.Identity (Identity)

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
  deriving (Show, Generic, Debug)


-- | User is a newtype wrapper over the token, which provies all necessary user information through
-- the JWT claim set. User has a set of virtual record fields that simplifies the access of the JWT claims
-- of `.idToken` of `Token`.
newtype User = User Token
  deriving (Show, Generic)


instance Debug User


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
