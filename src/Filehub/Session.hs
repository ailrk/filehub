module Filehub.Session where


import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Data.UUID.V4 qualified as UUID
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime, NominalDiffTime)
import Data.Time qualified as Time
import Filehub.Types (Session(..), SessionId(..))
import Filehub.Domain (SortFileBy(..))


createSessionId :: (IOE :> es) => Eff es SessionId
createSessionId = SessionId <$> liftIO UUID.nextRandom


-- | Create a expiry date 10 minutes from now.
createExpiryDate :: (IOE :> es) => Eff es UTCTime
createExpiryDate = do
  current <- liftIO Time.getCurrentTime
  pure $ secondsToNominalDiffTime (60 * 10) `addUTCTime` current


createSession :: (IOE :> es) => FilePath -> Eff es Session
createSession root =
  Session
  <$> createSessionId
  <*> createExpiryDate
  <*> pure root
  <*> pure ByName


extendSession :: NominalDiffTime -> Session -> Session
extendSession extension session = session { expireDate = extension `addUTCTime` session.expireDate  }

