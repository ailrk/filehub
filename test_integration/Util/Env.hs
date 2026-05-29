module Util.Env where

import Cache.InMemory qualified
import Control.Handle.Cache qualified as Cache
import Control.Handle.LockManager qualified as LockManager
import Data.ClientPath (Root(..), AbsPath(..))
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Time (secondsToNominalDiffTime)
import Data.UUID qualified as UUID
import Control.Handle.EvtLog qualified as EvtLog
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.Types.OIDC (OIDCAuthProviders(..))
import Filehub.Auth.Types.Simple (SimpleAuthUserDB(..))
import Filehub.Env (Env(..))
import Filehub.Locale (Locale(..))
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.SharedLink qualified as SharedLink
import Filehub.Types ( Theme(..) )
import GHC.Conc (newTVarIO)
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
  evtLogHandle   <- EvtLog.initialize ":memory:" 100
  let env =
        Env
          { port              = 0
          , theme             = Dark
          , sessionPool       = sessionPool
          , sessionDuration   = secondsToNominalDiffTime (60 * 60)
          , sharedLinkPool    = sharedLinkPool
          , targets           = targets
          , readOnly          = False
          , locale            = EN
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
          }
  pure env


nullLogger :: IO Logger
nullLogger = mkLogger "" $ \msg -> do
  putStrLn (Text.unpack $ "    " <> msg.lmMessage)
  pure ()


root :: FilePath
root = "/tmp/filehub-test/"


tid :: TargetId
tid = TargetId $ fromJust . UUID.fromString $ "11111111-35ad-49bb-b118-8e8fc24abf80"
