module Util.Env where

import Filehub.Prelude
import Cache.InMemory qualified
import Control.Handle.Cache qualified as Cache
import Control.Handle.LockManager qualified as LockManager
import Data.ClientPath (Root(..), AbsPath(..))
import Data.Maybe (fromJust)
import Data.UUID qualified as UUID
import Control.Handle.EvtLog qualified as EvtLog
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.Types.OIDC (OIDCAuthProviders(..))
import Filehub.Auth.Types.Simple (SimpleAuthUserDB(..))
import Filehub.Env (Env(..))
import Filehub.History qualified as History
import Filehub.Locale (Locale(..))
import Filehub.RateLimit qualified as RateLimit
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.SharedLink qualified as SharedLink
import Filehub.Theme (Theme(..))
import LockManager.Local qualified
import Log (mkLogger, LogMessage(..), Logger, LogLevel (..))
import Network.HTTP.Client.TLS (newTlsManager)
import Target.File (Target(..))
import Target.Types (TargetId(..), AnyTarget(..))


defaultEnv :: IO Env
defaultEnv = do
  sessionPool    <- Session.Pool.new
  activeUserPool <- ActiveUser.Pool.new
  logger         <- nullLogger
  httpManager    <- newTlsManager
  cache          <- Cache.makeInMemoryCache <$> Cache.InMemory.new 1000
  lockManager    <- LockManager.makeLocalLockManager <$> LockManager.Local.new
  targets        <- newTVarIO [ ( tid
                                , AnyTarget FileBackend
                                  { targetId   = tid
                                  , targetName = Nothing
                                  , root       = Root (AbsPath root)
                                  }
                                )
                              ]
  sharedLinkPool <- SharedLink.newShareLinkPool
  rateLimiters   <- RateLimit.newRateLimiters
  evtLogHandle   <- EvtLog.initialize ":memory:" 100
  history        <- History.newHistory
  themeVar       <- newTVarIO Dark
  localeVar      <- newTVarIO EN
  pluginsVar     <- newTVarIO []
  dynPluginsVar  <- newTVarIO []
  dynTargetsVar  <- newTVarIO []
  let env =
        Env
          { port              = 0
          , theme             = themeVar
          , sessionPool       = sessionPool
          , sessionDuration   = secondsToNominalDiffTime (60 * 60)
          , sharedLinkPool    = sharedLinkPool
          , targets           = targets
          , readOnly          = False
          , locale            = localeVar
          , logger            = logger
          , logLevel          = LogTrace
          , enableWAILog      = False
          , customThemeDark   = Nothing
          , customThemeLight  = Nothing
          , simpleAuthUserDB  = SimpleAuthUserDB mempty
          , oidcAuthProviders = OIDCAuthProviders mempty
          , httpManager       = httpManager
          , cache             = cache
          , lockManager       = lockManager
          , activeUsers       = activeUserPool
          , evtLogHandle      = evtLogHandle
          , history           = history
          , basePlugins       = []
          , plugins           = pluginsVar
          , dynamicPlugins    = dynPluginsVar
          , dynamicTargets    = dynTargetsVar
          , staticTargetIds   = set_singleton tid
          , persistDynConfig  = pure ()
          , dynamicConfigPath = Nothing
          , rateLimiters      = rateLimiters
          , pluginJsFiles     = []
          }
  pure env


nullLogger :: IO Logger
nullLogger = mkLogger "" $ \msg -> do
  putStrLn (txt_unpack $ "    " <> msg.lmMessage)
  pure ()


root :: FilePath
root = "/tmp/filehub-test/"


tid :: TargetId
tid = TargetId $ fromJust . UUID.fromString $ "11111111-35ad-49bb-b118-8e8fc24abf80"
