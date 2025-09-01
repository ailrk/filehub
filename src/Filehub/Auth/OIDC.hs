{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Auth.OIDC where

import Data.Text (Text)
import Servant (QueryParam, (:>), Proxy (..), Get, NoContent, Post, ReqBody, FormUrlEncoded, JSON)
import Servant.HTML.Lucid (HTML)
import GHC.Generics (Generic)
import Servant.Client (ClientM, client)
import Web.FormUrlEncoded (ToForm)
import Data.Aeson (FromJSON)
import Servant.Links (safeLink, Link)
import Network.URI (URI)


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
  , codeVerifier :: Maybe Text
  }
  deriving (Generic, Show)
instance ToForm TokenForm


data Authorization = Authorization
  { responseType        :: Text
  , clientId            :: Text
  , redirectUri         :: Text
  , scope               :: Text
  , state               :: Text
  , codeChallenge       :: Maybe Text
  , codeChallengeMethod :: Maybe Text
  }


data User = User
  { sub   :: Text
  , user  :: Text
  , email :: Maybe Text
  , token :: Token
  }
  deriving (Show, Eq)


data Provider = Provider
  { name         :: Text
  , issuer       :: URI
  , clientId     :: Text
  , clientSecret :: Text
  , grantType    :: Text
  , allowedUsers :: [Text]
  , redirectURI :: URI
  }
  deriving (Show, Eq)


newtype OIDCAuthProviders = OIDCAuthProviders [Provider]
  deriving (Show, Eq)


type AuthorizationApi = Servant.QueryParam "response_type" Text
                      Servant.:> Servant.QueryParam "client_id" Text
                      Servant.:> Servant.QueryParam "redirect_uri" Text
                      Servant.:> Servant.QueryParam "scope" Text
                      Servant.:> Servant.QueryParam "state" Text
                      Servant.:> Servant.QueryParam "code_challenge" Text
                      Servant.:> Servant.QueryParam "code_challenge_method" Text
                      Servant.:> Servant.Get '[HTML] Servant.NoContent


authorizeLink :: Authorization -> Link
authorizeLink (Authorization
  { responseType
  , clientId
  , redirectUri
  , scope
  , state
  , codeChallenge
  , codeChallengeMethod
  }) =
    authorizeLink'
      (Just responseType)
      (Just clientId)
      (Just redirectUri)
      (Just scope)
      (Just state)
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
