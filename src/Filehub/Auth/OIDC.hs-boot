module Filehub.Auth.OIDC where


data Provider

instance Eq Provider

newtype OIDCAuthProviders = OIDCAuthProviders [Provider]

data User
instance Eq User
instance Show User
