module Filehub.Session
  ( createSessionId
  , createExpiryDate
  , createSession
  , extendSession
  )
  where

import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Data.UUID.V4 qualified as UUID
import Data.Time (UTCTime, addUTCTime, NominalDiffTime)
import Data.Time qualified as Time
import Filehub.Types (Session(..), SessionId(..), Env(..))
import Filehub.Domain.Types (SortFileBy(..))
import {-# SOURCE #-} Filehub.Env qualified as Env


createSessionId :: (IOE :> es) => Eff es SessionId
createSessionId = SessionId <$> liftIO UUID.nextRandom


createExpiryDate :: (Reader Env :> es, IOE :> es) => Eff es UTCTime
createExpiryDate = do
  duration <- Env.getSessionDuration
  current <- liftIO Time.getCurrentTime
  pure $ duration `addUTCTime` current


createSession :: (Reader Env :> es, IOE :> es) => Eff es Session
createSession =
  Session
  <$> createSessionId
  <*> createExpiryDate
  <*> Env.getRoot
  <*> pure ByName


extendSession :: NominalDiffTime -> Session -> Session
extendSession extension session = session { expireDate = extension `addUTCTime` session.expireDate  }
