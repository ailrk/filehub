{-# LANGUAGE RoleAnnotations #-}
module Filehub.Auth.OIDC where


data Provider

instance Eq Provider

newtype OIDCAuthProviders = OIDCAuthProviders [Provider]

data User
instance Show User

type role OIDCFlow nominal
data OIDCFlow s

data SomeOIDCFlow = forall s . SomeOIDCFlow (OIDCFlow s)
