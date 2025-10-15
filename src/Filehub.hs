{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- The entrance of filehub.
module Filehub (main , mainDev)
  where

import Cache.InMemory
import Control.Exception (SomeException, throwIO)
import Data.Functor.Identity (Identity(..))
import Data.String.Interpolate (i)
import Data.Time (secondsToNominalDiffTime)
import Effectful (runEff, MonadIO (..))
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (runLog, logInfo_)
import EvtLog qualified
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
import Filehub.SharedLink qualified as SharedLink
import Lens.Micro.Platform ()
import LockRegistry.Local
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import Prelude hiding (init, readFile)
import Servant.Conduit ()
import System.Environment (withArgs)
import Target.File qualified as FS
import Target.S3 qualified as S3
import Target.Types (Target (..), getTargetId)
import UnliftIO (catch, hFlush, stdout)
import Effectful.Concurrent.STM (newTVarIO)


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

  env <- runEff . runConcurrent . runFileSystem  $ do
    sessionPool      <- Session.Pool.new
    activeUserPool   <- ActiveUser.Pool.new
    targets          <- runLog "targets" logger verbosity $ fromTargetConfig targetConfigs.unTargets >>= newTVarIO
    simpleAuthUserDB <- Auth.Simple.createSimpleAuthUserDB simpleAuthLoginUsers.unSimpleAuthUserRecords
    sharedLinkPool   <- SharedLink.newShareLinkPool
    lockRegistry     <- liftIO LockRegistry.Local.new
    cache            <- liftIO $ Cache.InMemory.new 5000
    httpManager      <- newTlsManager
    evtLogHandle     <- EvtLog.initialize "" 100
    pure Env
      { port              = port
      , theme             = theme
      , sessionPool       = sessionPool
      , sessionDuration   = secondsToNominalDiffTime (60 * 60)
      , targets           = targets
      , readOnly          = readOnly
      , locale            = locale
      , logger            = logger
      , logLevel          = verbosity
      , enableWAILog      = True
      , customThemeDark   = (.unCustomThemeDark) <$> customThemeDark
      , customThemeLight  = (.unCustomThemeLight) <$> customThemeLight
      , simpleAuthUserDB  = simpleAuthUserDB
      , oidcAuthProviders = OIDCAuthProviders (oidcAuthProviders.unOidcAuthProviders)
      , sharedLinkPool    = sharedLinkPool
      , activeUsers       = activeUserPool
      , httpManager       = httpManager
      , cache             = cache
      , lockRegistry      = lockRegistry
      , evtLogHandle      = evtLogHandle
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
        transform (FSTargetConfig c) = toAssocItem <$> FS.initialize c
        transform (S3TargetConfig c) = toAssocItem <$> S3.initialize c

        toAssocItem backend = (getTargetId (Target backend), Target backend)


-- | For developement with ghciwatch
--   Run `ghciwatch --test-ghci "Filehub.Entry.mainDev <your args for testing>"`
--   ghciwatch will watch file changes and rerun the server automatically.
mainDev :: String -> IO ()
mainDev args = withArgs (words args) main
