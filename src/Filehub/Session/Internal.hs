{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Filehub.Session.Internal
  ( createSession
  , extendSession
  , targetToSessionData
  )
  where

import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Time (UTCTime, addUTCTime, NominalDiffTime)
import Data.Time qualified as Time
import Data.Typeable (cast)
import Data.UUID.V4 qualified as UUID
import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Effectful.Reader.Dynamic (Reader, asks)
import Filehub.Types (Session(..), SessionId(..), Env(..), TargetSessionData (..), CopyState (..), Selected (..), SortFileBy(..), Layout(..))
import Filehub.UserAgent qualified as UserAgent
import Options.Applicative (asum)
import Target.File (FileSys, Target(..))
import Target.S3 (S3)
import Target.Types (AnyTarget (..))
import UnliftIO.STM (newTBQueueIO, newTVarIO)
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (readTVarIO)
import Data.Map.Strict qualified as Map
import Data.ClientPath (AbsPath(..), Root(..))


createSessionId :: (IOE :> es) => Eff es SessionId
createSessionId = SessionId <$> liftIO UUID.nextRandom


createExpireDate :: (Reader Env :> es, IOE :> es) => Eff es UTCTime
createExpireDate = do
  duration <- asks @Env (.sessionDuration)
  current  <- liftIO Time.getCurrentTime
  pure $ duration `addUTCTime` current


createSession :: (Reader Env :> es, Concurrent :> es, IOE :> es) => Eff es Session
createSession = do
  targets       <- asks @Env (.targets) >>= readTVarIO
  theme         <- asks @Env (.theme)
  locale        <- asks @Env (.locale)
  sessionId     <- createSessionId
  expireDate    <- createExpireDate
  notifications <- liftIO (newTBQueueIO 16)
  pendingTasks  <- newTVarIO Set.empty
  pure Session
    { sessionId         = sessionId
    , authId            = Nothing
    , sharedLinkPermit  = Nothing
    , resolution        = Nothing
    , deviceType        = UserAgent.Unknown
    , expireDate        = expireDate
    , targets           = targetToSessionData <$> Map.fromList targets
    , copyState         = NoCopyPaste
    , currentTargetId   = fst (head targets)
    , sidebarCollapsed  = False
    , layout            = ThumbnailLayout
    , theme             = theme
    , locale            = locale
    , oidcFlow          = Nothing
    , notifications     = notifications
    , pendingTasks      = pendingTasks
    }


targetToSessionData :: AnyTarget -> TargetSessionData
targetToSessionData (AnyTarget target) =
  fromMaybe defaultTargetSessionData . asum $
    [ cast target <&> \(x :: Target FileSys) -> defaultTargetSessionData { currentDir = coerce x.root }
    , cast target <&> \(_ :: Target S3)      -> defaultTargetSessionData
    ]
  where
    defaultTargetSessionData =
      TargetSessionData
        { currentDir   = AbsPath ""
        , sortedFileBy = ByNameUp
        , selected     = NoSelection
        }


extendSession :: NominalDiffTime -> Session -> Session
extendSession extension session = session { expireDate = extension `addUTCTime` session.expireDate  }
