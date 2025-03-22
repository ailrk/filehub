module Filehub.Session where

import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Data.UUID.V4 qualified as UUID
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime, NominalDiffTime)
import Data.Time qualified as Time
import Filehub.Types (Session(..), SessionId(..), Env(..))
import Filehub.Domain.Types (SortFileBy(..))
import {-# SOURCE #-} Filehub.Env qualified as Env


createSessionId :: (IOE :> es) => Eff es SessionId
createSessionId = SessionId <$> liftIO UUID.nextRandom


-- | Create a expiry date 10 minutes from now.
createExpiryDate :: (IOE :> es) => Eff es UTCTime
createExpiryDate = do
  current <- liftIO Time.getCurrentTime
  pure $ secondsToNominalDiffTime (60 * 10) `addUTCTime` current


createSession :: (Reader Env :> es, IOE :> es) => Eff es Session
createSession =
  Session
  <$> createSessionId
  <*> createExpiryDate
  <*> Env.getRoot
  <*> pure ByName


extendSession :: NominalDiffTime -> Session -> Session
extendSession extension session = session { expireDate = extension `addUTCTime` session.expireDate  }
