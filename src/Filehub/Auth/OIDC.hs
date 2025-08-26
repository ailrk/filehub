{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Auth.OIDC where


import Data.Text (Text)
import Data.Map.Strict (Map)


data Token = Token
  { idToken :: Text
  , accessToken :: Text
  , refreshToken :: Text
  , expiresIn :: Int
  , tokenType :: Text
  }
  deriving (Show, Eq)


data User = User
  { sub :: Text
  , user :: Text
  , email :: Maybe Text
  , token :: Token
  }
  deriving (Show, Eq)


newtype Issuer = Issuer Text deriving (Eq, Show, Ord)


data Provider = Provider
  { name :: Text
  , issuer :: Text
  , clientId :: Text
  , clientSecret :: Text
  , grantType :: Text
  , allowedUsers :: [Text]
  , redirectURIs :: [Text]
  }
  deriving (Show, Eq)


newtype OIDCAuthProviders = OIDCAuthProviders (Map Issuer Provider)
