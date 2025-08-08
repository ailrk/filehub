module Filehub.Session
  ( createSession
  , extendSession
  )
  where

import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Data.UUID.V4 qualified as UUID
import Data.Time (UTCTime, addUTCTime, NominalDiffTime)
import Data.Time qualified as Time
import Filehub.Types (Session(..), SessionId(..), Env(..), Target (..), TargetSessionData (..), CopyState (..), Selected (..), SortFileBy(..))
import Filehub.Env.Internal qualified as Env
import Filehub.UserAgent qualified as UserAgent
import Data.Typeable (cast)
import Filehub.Target.S3 (S3, Backend(..))
import Filehub.Target.File (FileSys, Backend(..))
import Data.Maybe (fromMaybe)
import Options.Applicative (asum)
import Data.Functor ((<&>))
import Filehub.Layout (Layout(..))


createSessionId :: (IOE :> es) => Eff es SessionId
createSessionId = SessionId <$> liftIO UUID.nextRandom


createExpireDate :: (Reader Env :> es, IOE :> es) => Eff es UTCTime
createExpireDate = do
  duration <- Env.getSessionDuration
  current <- liftIO Time.getCurrentTime
  pure $ duration `addUTCTime` current


createSession :: (Reader Env :> es, IOE :> es) => Eff es Session
createSession = do
  targets <- Env.getTargets
  sessionId <- createSessionId
  expireDate <- createExpireDate
  pure Session
    { sessionId = sessionId
    , resolution = Nothing
    , deviceType = UserAgent.Unknown
    , expireDate = expireDate
    , targets = targetToSessionData <$> targets
    , copyState = NoCopyPaste
    , index = 0
    , layout = ThumbnailLayout
    }


targetToSessionData :: Target -> TargetSessionData
targetToSessionData (Target target) =
  fromMaybe defaultTargetSessionData . asum $
    [ cast target <&> \(x :: Backend FileSys) -> defaultTargetSessionData { currentDir = x.root }
    , cast target <&> \(_ :: Backend S3) -> defaultTargetSessionData
    ]
  where
    defaultTargetSessionData =
      TargetSessionData
        { currentDir = ""
        , sortedFileBy = ByNameUp
        , selected = NoSelection
        }


extendSession :: NominalDiffTime -> Session -> Session
extendSession extension session = session { expireDate = extension `addUTCTime` session.expireDate  }
