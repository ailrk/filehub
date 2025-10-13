{-# LANGUAGE RoleAnnotations #-}
module Filehub.Auth.OIDC where
import Text.Debug (Debug)


data Provider

instance Eq Provider

newtype OIDCAuthProviders = OIDCAuthProviders [Provider]

data User
instance Show User
instance Debug User

type role OIDCFlow nominal
data OIDCFlow s

data SomeOIDCFlow = forall s . SomeOIDCFlow (OIDCFlow s)
