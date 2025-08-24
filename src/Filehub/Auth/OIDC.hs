module Filehub.Auth.OIDC where


import Network.URI (URI)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Set (Set)


newtype OIDCClientId = OIDCClientId Text deriving (Show, Eq)
newtype OIDCClientSecret = OIDCClientSecret Text deriving (Show, Eq)
newtype OIDCSub = OIDCSub Text deriving (Show, Eq)

newtype OIDCAllowedUsers = OIDCAllowedUsers (Set OIDCSub) deriving (Show, Eq)

data OIDCUser = OIDCUser
  { user :: Text
  , sub :: OIDCSub
  , email :: Maybe Text
  }
  deriving (Show, Eq)


newtype OIDCUserDB = OIDCUserDB (Map OIDCSub OIDCUser) deriving (Show, Eq)


data OIDCProvider = OIDCProvider
  { name :: Text
  , issuer :: URI
  , clientId :: OIDCClientId
  , clientSecret :: OIDCClientSecret
  , allowedUser :: [Text]
  , redirectURI :: URI
  }
  deriving (Show, Eq)
