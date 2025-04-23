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
import Filehub.Types (Session(..), SessionId(..), Env(..), Target (..), TargetSessionData (..), FileTarget(..), CopyState (..), Selected (..), SortFileBy(..))
import Filehub.Env.Internal qualified as Env


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
    , expireDate = expireDate
    , targets = targetToSessionData <$> targets
    , index = 0
    }
  where
    targetToSessionData :: Target -> TargetSessionData
    targetToSessionData (S3Target _) =
      TargetSessionData
        { currentDir = ""
        , sortedFileBy = ByNameUp
        , copyState = NoCopyPaste
        , selected = NoSelection
        }
    targetToSessionData (FileTarget target) =
      TargetSessionData
        { currentDir = target.root
        , sortedFileBy = ByNameUp
        , copyState = NoCopyPaste
        , selected = NoSelection
        }


extendSession :: NominalDiffTime -> Session -> Session
extendSession extension session = session { expireDate = extension `addUTCTime` session.expireDate  }
