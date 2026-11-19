module Filehub.Config
  ( Config(..)
  , TargetConfig(..)
  , Targets(..)
  , CustomThemeDark(..)
  , CustomThemeLight(..)
  , SimpleAuthUserRecords(..)
  , OidcAuthProviders(..)
  , merge
  )
  where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Functor.Identity
import Filehub.Auth.Types.OIDC qualified as Auth.OIDC
import Filehub.Auth.Types.Simple qualified as Auth.Simple
import Filehub.Locale (Locale (..))
import Filehub.Plugin.Types (StaticPlugins)
import Filehub.Theme (Theme (..), CustomTheme)
import Target.File (FileSys)
import Target.Types qualified as Target
import Target.S3 (S3)
import Log (LogLevel (..))


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
  , plugins               :: StaticPlugins
  }


-- | New type wrappers for each config.
newtype Targets               = Targets               { unTargets               :: [TargetConfig] }           deriving newtype (Semigroup, Monoid)
newtype SimpleAuthUserRecords = SimpleAuthUserRecords { unSimpleAuthUserRecords :: [Auth.Simple.UserRecord] } deriving newtype (Semigroup, Monoid)
newtype OidcAuthProviders     = OidcAuthProviders     { unOidcAuthProviders     :: [Auth.OIDC.Provider] }     deriving newtype (Semigroup, Monoid)
newtype CustomThemeDark       = CustomThemeDark       { unCustomThemeDark       :: CustomTheme }              deriving (Show)
newtype CustomThemeLight      = CustomThemeLight      { unCustomThemeLight      :: CustomTheme }              deriving (Show)


data TargetConfig
  = FSTargetConfig (Target.Config FileSys)
  | S3TargetConfig (Target.Config S3)
  deriving (Show, Eq)


-- | Later config wins for scalar fields; list fields concatenate.
instance Semigroup (Config Maybe) where
  a <> b = Config
    { port                  = b.port <|> a.port
    , theme                 = b.theme <|> a.theme
    , verbosity             = b.verbosity <|> a.verbosity
    , readOnly              = b.readOnly <|> a.readOnly
    , locale                = b.locale <|> a.locale
    , customThemeDark       = b.customThemeDark <|> a.customThemeDark
    , customThemeLight      = b.customThemeLight <|> a.customThemeLight
    , targets               = a.targets <> b.targets
    , simpleAuthUserRecords = a.simpleAuthUserRecords <> b.simpleAuthUserRecords
    , oidcAuthProviders     = a.oidcAuthProviders <> b.oidcAuthProviders
    , plugins               = a.plugins <> b.plugins
    }

instance Monoid (Config Maybe) where
  mempty = Config Nothing Nothing Nothing Nothing Nothing Nothing Nothing mempty mempty mempty mempty


-- | The second config overrides the first one.
merge :: Config Maybe -> Config Maybe -> Either String (Config Identity)
merge cfg1 cfg2 = do
  port             <- maybe (Left "port is missing") Right (cfg2.port <|> cfg1.port)
  theme            <- maybe (Right Dark)             Right (cfg2.theme <|> cfg1.theme)
  verbosity        <- maybe (Right LogInfo)          Right (cfg2.verbosity <|> cfg1.verbosity)
  readOnly         <- maybe (Right True)             Right (cfg2.readOnly <|> cfg1.readOnly)
  locale           <- maybe (Right EN)               Right (cfg2.locale <|> cfg1.locale)
  let customThemeDark      = (join cfg2.customThemeDark <|> join cfg1.customThemeDark)
  let customThemeLight     = (join cfg2.customThemeLight <|> join cfg1.customThemeLight)
  let targets              = mconcat [cfg1.targets, cfg2.targets]
  let simpleAuthLoginUsers = mconcat [cfg1.simpleAuthUserRecords, cfg2.simpleAuthUserRecords]
  let oidcAuthProviders    = mconcat [cfg1.oidcAuthProviders , cfg2.oidcAuthProviders]
  let plugins              = mconcat [cfg1.plugins, cfg2.plugins]
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
      , plugins               = plugins
      }
