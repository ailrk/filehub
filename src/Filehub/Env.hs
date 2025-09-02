-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module defines the top level `Env` record that holds all the application
-- configuration and states. `Env` is accessed through the top level Reader effect.
-- Every new piece of configuration or state that needs to be accessed by the
-- `Filehub` effect should be added here.
module Filehub.Env
  ( Env(..)
  , hasNoLogin
  )
  where

import Data.Map.Strict qualified as Map
import Lens.Micro.Platform ()
import Filehub.Auth.Simple (SimpleAuthUserDB(..))
import Filehub.Auth.OIDC (OIDCAuthProviders(..))
import Data.Time (NominalDiffTime)
import Filehub.Theme (Theme)
import Filehub.Session.Types qualified as Session
import Filehub.Target.Types (Target)
import Filehub.Auth.Types (ActiveUsers(..))
import Log (Logger, LogLevel)
import Filehub.Locale (Locale)


data Env = Env
  { -- The port number the filehub listens on.
    port              :: !Int
    -- The initial theme. Each session has there own theme as well.
  , theme             :: Theme
    -- The session pool that maps from `SessionId` to `session`
  , sessionPool       :: Session.Pool
    -- Session duration setting. An idle session will expire after the duration.
  , sessionDuration   :: NominalDiffTime
    -- List of targets. `Target` is existential, it hides different target backends.
  , targets           :: [Target]
    -- Top level readonly setting.
  , readOnly          :: Bool
    -- Top level logging facility.
  , logger            :: Logger
    -- Log level configuaration
  , logLevel          :: LogLevel
    -- Locale. e.g EN/ZH_CN
  , locale            :: Locale
    -- User credentials for simple auth login mechanism.
  , simpleAuthUserDB  :: SimpleAuthUserDB
    -- OIDC configurations for OIDC loging mechanism
  , oidcAuthProviders :: OIDCAuthProviders
    -- Map from `AuthId` to `ActiveUser`. The definition of an active user depends on
    -- its login method. An active user can have multiple sessions.
  , activeUsers       :: ActiveUsers
  }


-- | Check if there is login information provided when the program starts.
hasNoLogin :: Env -> Bool
hasNoLogin (Env { simpleAuthUserDB = SimpleAuthUserDB db
                , oidcAuthProviders = OIDCAuthProviders providers}) = Map.null db && null providers
