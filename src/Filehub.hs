{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- The entrance of filehub.
module Filehub where

import Cache.InMemory qualified as Cache.InMemory
import Control.Exception (SomeException)
import Control.Exception (throwIO)
import Data.Functor.Identity (Identity(..))
import Data.String.Interpolate (i)
import Data.Time (secondsToNominalDiffTime)
import Effectful (runEff)
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (logInfo_)
import Effectful.Log (runLog)
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.OIDC (OIDCAuthProviders(..))
import Filehub.Auth.Simple qualified as Auth.Simple
import Filehub.Config (Config(..), TargetConfig (..))
import Filehub.Config qualified as Config
import Filehub.Config.Options (Options(..))
import Filehub.Config.Options qualified as Config.Options
import Filehub.Config.Toml qualified as Config.Toml
import Filehub.Env (Env(..))
import Filehub.Log qualified as Log
import Filehub.Orphan ()
import Filehub.Server (application)
import Filehub.Session.Pool qualified as Session.Pool
import Lens.Micro.Platform ()
import LockRegistry.Local qualified as LockRegistry.Local
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import Prelude hiding (init, readFile)
import Servant.Conduit ()
import System.Environment (withArgs)
import Target.File qualified as FS
import Target.S3 qualified as S3
import Target.Types (Target (..))
import UnliftIO (catch)
import UnliftIO (hFlush, stdout)


main :: IO ()
main = Log.withColoredStdoutLogger \logger -> do
  Options
    { configFile = configFile
    , optionConfig
    } <- Config.Options.parseOptions
  config <- Config.Toml.parseConfigFile configFile
  Config
    { port                  = Identity port
    , theme                 = Identity theme
    , verbosity             = Identity verbosity
    , readOnly              = Identity readOnly
    , locale                = Identity locale
    , customThemeDark       = Identity customThemeDark
    , customThemeLight      = Identity customThemeLight
    , targets               = targetConfigs
    , simpleAuthUserRecords = simpleAuthLoginUsers
    , oidcAuthProviders     = oidcAuthProviders
    } <-
      either
        (\err -> throwIO (userError err))
        pure
        (Config.merge optionConfig config)

  runEff $ runLog "main" logger verbosity do
    logInfo_ [i|port:      #{port}|]
    logInfo_ [i|theme:     #{theme}|]
    logInfo_ [i|verbosity: #{verbosity}|]
    logInfo_ [i|readonly:  #{readOnly}|]
    logInfo_ [i|locale:    #{locale}|]
#ifdef DEBUG
    logInfo_ [i|debug:     true|]
#endif

  sessionPool      <- runEff Session.Pool.new
  activeUserPool   <- runEff ActiveUser.Pool.new
  targets          <- runEff . runLog "targets" logger verbosity . runFileSystem $ fromTargetConfig targetConfigs.unTargets
  simpleAuthUserDB <- runEff . runFileSystem $ Auth.Simple.createSimpleAuthUserDB simpleAuthLoginUsers.unSimpleAuthUserRecords
  lockRegistry     <- LockRegistry.Local.new
  cache            <- Cache.InMemory.new 5000
  httpManager      <- newTlsManager

  let env =
        Env
          { port              = port
          , theme             = theme
          , sessionPool       = sessionPool
          , sessionDuration   = secondsToNominalDiffTime (60 * 60)
          , targets           = targets
          , readOnly          = readOnly
          , locale            = locale
          , logger            = logger
          , logLevel          = verbosity
          , customThemeDark   = (.unCustomThemeDark) <$> customThemeDark
          , customThemeLight  = (.unCustomThemeLight) <$> customThemeLight
          , simpleAuthUserDB  = simpleAuthUserDB
          , oidcAuthProviders = OIDCAuthProviders (oidcAuthProviders.unOidcAuthProviders)
          , activeUsers       = activeUserPool
          , httpManager       = httpManager
          , cache             = cache
          , lockRegistry      = lockRegistry
          }

  go env `catch` handler
  where
    go env = do
      putStr "[Filehub server is up and running]\n" >> hFlush stdout
      let settings = setPort env.port defaultSettings
      runSettings settings (application env)
    handler (e :: SomeException) = putStrLn ("server is down " <> show e) >> hFlush stdout

    fromTargetConfig opts = traverse transform opts
      where
        transform (FSTargetConfig c) = Target <$> FS.initialize c
        transform (S3TargetConfig c) = Target <$> S3.initialize c


-- | For developement with ghciwatch
--   Run `ghciwatch --test-ghci "Filehub.Entry.mainDev <your args for testing>"`
--   ghciwatch will watch file changes and rerun the server automatically.
mainDev :: String -> IO ()
mainDev args = withArgs (words args) main
