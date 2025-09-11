{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Filehub.Auth.Types
  ( AuthId(..)
  , Auth(..)
  , createAuthId
  )
  where

import Prelude hiding (readFile)
import {-# SOURCE #-} Filehub.Auth.Simple qualified as Auth.Simple
import {-# SOURCE #-} Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Auth.Types.AuthId (AuthId(..), createAuthId)


data Auth
  = Simple Auth.Simple.Username
  | OIDC Auth.OIDC.User
