module Filehub.Session.Internal
  ( createSession
  , extendSession
  , targetToSessionData
  )
  where

import Control.Monad.Reader (asks)
import Data.ClientPath (AbsPath(..), Root(..))
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Time (UTCTime, addUTCTime, NominalDiffTime)
import Data.Time qualified as Time
import Data.Typeable (cast)
import Data.UUID.V4 qualified as UUID
import Filehub.Monad (Filehub)
import Filehub.Session.Types (Session(..), TargetSessionData(..), Selected (..), CopyState (..), Layout (..))
import Filehub.Session.Types.SessionId (SessionId(..))
import Filehub.Types (SortFileBy (..), Env(..))
import Filehub.UserAgent qualified as UserAgent
import Options.Applicative (asum)
import Target.File (FileSys, Target(..))
import Target.S3 (S3)
import Target.Types (AnyTarget (..))
import UnliftIO (MonadIO(..))
import UnliftIO.STM (newTBQueueIO, newTVarIO, readTVarIO)


createSessionId :: Filehub SessionId
createSessionId = SessionId <$> liftIO UUID.nextRandom


createExpireDate :: Filehub UTCTime
createExpireDate = do
  duration <- asks (.sessionDuration)
  current  <- liftIO Time.getCurrentTime
  pure $ duration `addUTCTime` current


createSession :: Filehub Session
createSession = do
  targets       <- asks (.targets) >>= readTVarIO
  theme         <- asks (.theme)
  locale        <- asks (.locale)
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
        , sortedFileBy = ByModifiedDown
        , selected     = NoSelection
        }


extendSession :: NominalDiffTime -> Session -> Session
extendSession extension session = session { expireDate = extension `addUTCTime` session.expireDate  }
