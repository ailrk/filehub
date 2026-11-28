{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- The entrance of filehub.
module Filehub (main , mainDev)
  where

import Filehub.Prelude
import Cache.InMemory
import Data.ClientPath (Root(..), AbsPath(..))
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.Types.OIDC (OIDCAuthProviders(..))
import Filehub.Auth.Simple qualified as Auth.Simple
import Filehub.Config (Config(..), TargetConfig (..), Targets(..))
import Filehub.Plugin.Types (StaticPlugins(..), StaticPlugin(..))
import Filehub.Config qualified as Config
import Filehub.Config.Options (Options(..))
import Filehub.Config.Options qualified as Config.Options
import Filehub.Config.Toml qualified as Config.Toml
import Filehub.Env (Env(..))
import Filehub.History qualified as History
import Filehub.RateLimit qualified as RateLimit
import Filehub.Locale (Locale)
import Filehub.Log qualified as Log
import Filehub.Orphan ()
import Filehub.Server (application)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.SharedLink qualified as SharedLink
import Filehub.Theme (Theme)
import LockManager.Local qualified
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import Prelude hiding (init, readFile)
import Servant.Conduit ()
import System.Directory (doesFileExist)
import System.Environment (withArgs)
import Target.File qualified as FS
import Target.File (Target(..), FileSys)
import Target.S3 qualified as S3
import Target.S3 (Target(..), S3)
import Target.Types (AnyTarget (..), getTargetId, handleTarget, targetHandler)
import UnliftIO (hFlush, stdout, hSetBuffering, BufferMode (..), stderr)
import Log (logInfo_, runLogT)
import Control.Handle.Cache qualified as Cache
import Control.Handle.EvtLog qualified as EvtLog
import Control.Handle.LockManager qualified as LockManager


main :: IO ()
main = Log.withColoredStdoutLogger \logger -> do
  Options
    { configFiles        = configFiles
    , optionConfig
    , dynamicConfigPath
    } <- Config.Options.parseOptions

  -- Load static config files (merged, later wins for scalars)
  fileConfigs <- traverse (Config.Toml.parseConfigFile . Just) configFiles
  let fileConfig = mconcat fileConfigs

  -- Load dynamic config (created/managed by settings UI); empty if file doesn't exist
  dynFileConfig <- parseDynamicConfig dynamicConfigPath

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
    , plugins               = plugins
    , pluginJsFiles         = pluginJsFilePaths
    } <-
      either
        (\err -> throwIO (userError err))
        pure
        (Config.merge optionConfig (fileConfig <> dynFileConfig))

  jsFileContents <- traverse txt_readFile pluginJsFilePaths

  when (null targetConfigs.unTargets) do
    throwIO (userError "No target specified")

  runLogT "main" logger verbosity do
    logInfo_ [i|port:      #{port}|]
    logInfo_ [i|theme:     #{theme}|]
    logInfo_ [i|verbosity: #{verbosity}|]
    logInfo_ [i|readonly:  #{readOnly}|]
    logInfo_ [i|locale:    #{locale}|]
    let StaticPlugins ps = plugins
        pluginNames = [ n | StaticPlugin { name = n } <- ps ]
    logInfo_ [i|plugins:   #{pluginNames :: [Text]}|]
    logInfo_ [i|plugin_js: #{pluginJsFilePaths}|]
#ifdef DEBUG
    logInfo_ [i|debug:     true|]
#endif

  env <- do
    sessionPool    <- Session.Pool.new
    activeUserPool <- ActiveUser.Pool.new

    -- Static targets from CLI + file config
    targetsList    <- runLogT "targets" logger verbosity
                        $ fromTargetConfig targetConfigs.unTargets
    let staticTids = set_fromList (fst <$> targetsList)

    -- Dynamic targets previously saved by the settings UI
    dynTargetsList <- runLogT "targets" logger verbosity
                        $ fromTargetConfig dynFileConfig.targets.unTargets
    targetsVar     <- newTVarIO (targetsList ++ dynTargetsList)
    dynTargetsVar  <- newTVarIO (snd <$> dynTargetsList)

    let basePlugins = plugins.unStaticPlugins
    let dynPlugins  = dynFileConfig.plugins.unStaticPlugins
    pluginsVar      <- newTVarIO (basePlugins ++ dynPlugins)
    dynPluginsVar   <- newTVarIO dynPlugins

    themeVar   <- newTVarIO theme
    localeVar  <- newTVarIO locale

    simpleAuthUserDB <- Auth.Simple.createSimpleAuthUserDB simpleAuthLoginUsers.unSimpleAuthUserRecords
    sharedLinkPool   <- SharedLink.newShareLinkPool
    lockManager      <- LockManager.makeLocalLockManager <$> LockManager.Local.new
    cache            <- Cache.makeInMemoryCache <$> Cache.InMemory.new 5000
    httpManager      <- newTlsManager
    evtLogHandle     <- EvtLog.initialize "" 100
    history          <- History.newHistory
    rateLimiters     <- RateLimit.newRateLimiters

    let persistCallback = buildPersistCallback
          dynamicConfigPath themeVar localeVar dynPluginsVar dynTargetsVar

    pure Env
      { port              = port
      , theme             = themeVar
      , sessionPool       = sessionPool
      , sessionDuration   = time_fromSeconds (60 * 60)
      , targets           = targetsVar
      , readOnly          = readOnly
      , locale            = localeVar
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
      , lockManager       = lockManager
      , evtLogHandle      = evtLogHandle
      , history           = history
      , basePlugins       = basePlugins
      , plugins           = pluginsVar
      , dynamicPlugins    = dynPluginsVar
      , dynamicTargets    = dynTargetsVar
      , staticTargetIds   = staticTids
      , persistDynConfig  = persistCallback
      , dynamicConfigPath = dynamicConfigPath
      , rateLimiters      = rateLimiters
      , pluginJsFiles     = jsFileContents
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

        toAssocItem backend = (getTargetId (AnyTarget backend), AnyTarget backend)


-- | Load the dynamic config file; return mempty if the file does not exist yet.
parseDynamicConfig :: Maybe FilePath -> IO (Config Maybe)
parseDynamicConfig Nothing     = pure mempty
parseDynamicConfig (Just path) = do
  exists <- doesFileExist path
  if exists
    then Config.Toml.parseConfigFile (Just path)
    else pure mempty


-- | Build the callback that serialises the current dynamic state to disk.
buildPersistCallback
  :: Maybe FilePath
  -> TVar Theme
  -> TVar Locale
  -> TVar [StaticPlugin]
  -> TVar [AnyTarget]
  -> IO ()
buildPersistCallback Nothing _ _ _ _ = pure ()
buildPersistCallback (Just path) themeVar localeVar dynPluginsVar dynTargetsVar = do
  themeVal   <- readTVarIO themeVar
  localeVal  <- readTVarIO localeVar
  dynPlugins <- readTVarIO dynPluginsVar
  dynTargets <- readTVarIO dynTargetsVar
  let dynCfg = (mempty :: Config Maybe)
        { theme   = Just themeVal
        , locale  = Just localeVal
        , plugins = StaticPlugins dynPlugins
        , targets = Targets (extractTargetConfig <$> dynTargets)
        }
  txt_writeFile path (Config.Toml.serializeConfig dynCfg)


-- | Reconstruct a TargetConfig from an initialised AnyTarget for persistence.
extractTargetConfig :: AnyTarget -> TargetConfig
extractTargetConfig t =
  fromMaybe (error "extractTargetConfig: unrecognised target backend") $
    handleTarget t
      [ targetHandler @FileSys \(FileBackend { root = Root (AbsPath fp) }) ->
            FSTargetConfig (FS.Config { root = fp })
      , targetHandler @S3 \(S3Backend { bucket = b }) ->
            S3TargetConfig (S3.Config { bucket = txt_unpack b })
      ]


-- | For developement with ghciwatch
--   Run `ghciwatch --test-ghci "Filehub.Entry.mainDev <your args for testing>"`
--   ghciwatch will watch file changes and rerun the server automatically.
mainDev :: String -> IO ()
mainDev args = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  withArgs (words args) main
