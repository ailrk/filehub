{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Auth.OIDC where


import Data.Text (Text)


data Token = Token
  { idToken      :: Text
  , accessToken  :: Text
  , refreshToken :: Text
  , expiresIn    :: Int
  , tokenType    :: Text
  }
  deriving (Show, Eq)


data User = User
  { sub   :: Text
  , user  :: Text
  , email :: Maybe Text
  , token :: Token
  }
  deriving (Show, Eq)


data Provider = Provider
  { name         :: Text
  , issuer       :: Text
  , clientId     :: Text
  , clientSecret :: Text
  , grantType    :: Text
  , allowedUsers :: [Text]
  , redirectURIs :: [Text]
  }
  deriving (Show, Eq)


newtype OIDCAuthProviders = OIDCAuthProviders [Provider]
  deriving (Show, Eq)



-- | Exchange token with the IdP
exchangeToken = undefined



-- | Verify ID token
verifyToken = undefined
