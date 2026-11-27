module Filehub.Auth.Types
  ( AuthId(..)
  , Auth(..)
  , createAuthId
  )
  where

import Filehub.Prelude
import Prelude hiding (readFile)
import Filehub.Auth.Types.Simple qualified as Auth.Simple
import Filehub.Auth.Types.OIDC qualified as Auth.OIDC
import Filehub.Auth.Types.AuthId (AuthId(..), createAuthId)
import Text.Debug (Debug)


data Auth
  = Simple Auth.Simple.Username
  | OIDC Auth.OIDC.User
  deriving (Generic, Debug)
