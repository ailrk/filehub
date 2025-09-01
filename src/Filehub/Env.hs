-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module defines some helpers on `Env`. All functions should be pure and simply
-- take `Env` and tell some facts about it.
module Filehub.Env
  ( Env(..)
  , hasNoLogin
  )
  where

import Data.Map.Strict qualified as Map
import Lens.Micro.Platform ()
import Filehub.Types (Env(..))
import Filehub.Auth.Simple (SimpleAuthUserDB(..))
import Filehub.Auth.OIDC (OIDCAuthProviders(..))


-- | Check if there is login information provided when the program starts.
hasNoLogin :: Env -> Bool
hasNoLogin (Env { simpleAuthUserDB = SimpleAuthUserDB db
                , oidcAuthProviders = OIDCAuthProviders providers}) = Map.null db && null providers
