module Filehub.Session.Internal
  ( createSession
  , extendSession
  )
  where

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
import Target.File (FileSys, Backend(..))
import Target.S3 (S3)
import Target.Types (Target (..))
import UnliftIO.STM (newTBQueueIO, newTVarIO)


createSessionId :: (IOE :> es) => Eff es SessionId
createSessionId = SessionId <$> liftIO UUID.nextRandom


createExpireDate :: (Reader Env :> es, IOE :> es) => Eff es UTCTime
createExpireDate = do
  duration <- asks @Env (.sessionDuration)
  current  <- liftIO Time.getCurrentTime
  pure $ duration `addUTCTime` current


createSession :: (Reader Env :> es, IOE :> es) => Eff es Session
createSession = do
  targets       <- asks @Env (.targets)
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
    , targets           = targetToSessionData <$> targets
    , copyState         = NoCopyPaste
    , index             = 0
    , layout            = ThumbnailLayout
    , theme             = theme
    , locale            = locale
    , oidcFlow          = Nothing
    , notifications     = notifications
    , pendingTasks      = pendingTasks
    }


targetToSessionData :: Target -> TargetSessionData
targetToSessionData (Target target) =
  fromMaybe defaultTargetSessionData . asum $
    [ cast target <&> \(x :: Backend FileSys) -> defaultTargetSessionData { currentDir = x.root }
    , cast target <&> \(_ :: Backend S3)      -> defaultTargetSessionData
    ]
  where
    defaultTargetSessionData =
      TargetSessionData
        { currentDir   = ""
        , sortedFileBy = ByNameUp
        , selected     = NoSelection
        }


extendSession :: NominalDiffTime -> Session -> Session
extendSession extension session = session { expireDate = extension `addUTCTime` session.expireDate  }
