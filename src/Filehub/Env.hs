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
import Data.Time (NominalDiffTime)
import {-# SOURCE #-} Filehub.Auth.Simple (SimpleAuthUserDB(..))
import Filehub.ActiveUser.Types qualified as ActiveUser
import Filehub.Locale (Locale)
import Filehub.Session.Types qualified as Session
import Filehub.Target.Types (Target)
import Filehub.Theme (Theme, CustomTheme)
import Lens.Micro.Platform ()
import Log (Logger, LogLevel)
import Network.HTTP.Client qualified as HTTP
import {-# SOURCE #-} Filehub.Auth.OIDC (OIDCAuthProviders(..))


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
  , activeUsers       :: ActiveUser.Pool
    -- top level http-tls manager
  , httpManager       :: HTTP.Manager
    -- dark custom theme
  , customThemeDark   :: Maybe CustomTheme
    -- light custom theme
  , customThemeLight  :: Maybe CustomTheme
  }


-- | Check if there is login information provided when the program starts.
hasNoLogin :: Env -> Bool
hasNoLogin (Env { simpleAuthUserDB = SimpleAuthUserDB db
                , oidcAuthProviders = OIDCAuthProviders providers}) = Map.null db && null providers
