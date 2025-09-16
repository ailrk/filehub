{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Config where

import Filehub.Theme (Theme (..), CustomTheme)
import {-# SOURCE #-} Filehub.Auth.Simple qualified as Auth.Simple
import Effectful.Log (LogLevel (..))
import Data.Functor.Identity
import Control.Applicative ((<|>))
import {-# SOURCE #-} Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Locale (Locale (..))
import Control.Monad (join)
import Target.File qualified
import Target.S3 qualified


data Config f = Config
  { port                  :: f Int
  , theme                 :: f Theme
  , verbosity             :: f LogLevel
  , readOnly              :: f Bool
  , locale                :: f Locale
  , customThemeDark       :: f (Maybe CustomThemeDark)
  , customThemeLight      :: f (Maybe CustomThemeLight)
  , targets               :: Targets
  , simpleAuthUserRecords :: SimpleAuthUserRecords
  , oidcAuthProviders     :: OidcAuthProviders
  }


-- | New type wrappers for each config.
newtype Targets               = Targets               { unTargets               :: [TargetConfig] }           deriving (Semigroup, Monoid)
newtype SimpleAuthUserRecords = SimpleAuthUserRecords { unSimpleAuthUserRecords :: [Auth.Simple.UserRecord] } deriving (Semigroup, Monoid)
newtype OidcAuthProviders     = OidcAuthProviders     { unOidcAuthProviders     :: [Auth.OIDC.Provider] }     deriving (Semigroup, Monoid)
newtype CustomThemeDark       = CustomThemeDark       { unCustomThemeDark       :: CustomTheme }              deriving (Show)
newtype CustomThemeLight      = CustomThemeLight      { unCustomThemeLight      :: CustomTheme }              deriving (Show)


data TargetConfig
  = FSTargetConfig Target.File.Config
  | S3TargetConfig Target.S3.Config
  deriving (Show, Eq)


-- | The second config overrides the first one.
merge :: Config Maybe -> Config Maybe -> Either String (Config Identity)
merge cfg1 cfg2 = do
  port             <- maybe (Left "port is missing") Right (cfg2.port <|> cfg1.port)
  theme            <- maybe (Right Dark)             Right (cfg2.theme <|> cfg1.theme)
  verbosity        <- maybe (Right LogInfo)          Right (cfg2.verbosity <|> cfg1.verbosity)
  readOnly         <- maybe (Right True)             Right (cfg2.readOnly <|> cfg1.readOnly)
  locale           <- maybe (Right EN)               Right (cfg2.locale <|> cfg1.locale)
  customThemeDark  <- pure                                 (join cfg2.customThemeDark <|> join cfg1.customThemeDark)
  customThemeLight <- pure                                 (join cfg2.customThemeLight <|> join cfg1.customThemeLight)
  let targets              = mconcat [cfg1.targets, cfg2.targets]
  let simpleAuthLoginUsers = mconcat [cfg1.simpleAuthUserRecords, cfg2.simpleAuthUserRecords]
  let oidcAuthProviders    = mconcat [cfg1.oidcAuthProviders , cfg2.oidcAuthProviders]
  pure
    Config
      { port                  = Identity port
      , theme                 = Identity theme
      , verbosity             = Identity verbosity
      , readOnly              = Identity readOnly
      , locale                = Identity locale
      , customThemeDark       = Identity customThemeDark
      , customThemeLight      = Identity customThemeLight
      , targets               = targets
      , simpleAuthUserRecords = simpleAuthLoginUsers
      , oidcAuthProviders     = oidcAuthProviders
      }
