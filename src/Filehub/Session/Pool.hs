module Filehub.Session.Pool
  ( Session.Pool(..)
  , new
  , newSession
  , extendSession
  , delete
  , get
  , withSession
  , update
  )
  where

import Data.Time (addUTCTime)
import Data.Time.Clock qualified as Time
import Data.Map.Strict qualified as M
import Control.Concurrent.Timer qualified as Timer
import Control.Concurrent.Suspend qualified as Suspend
import Filehub.Types (Env(..))
import Filehub.Session.Internal qualified as Session
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Session.Types (Session(..), SessionId)
import Filehub.Session.Types qualified as Session
import Filehub.Monad (Filehub)
import Control.Monad.Reader (asks)
import UnliftIO (MonadIO(..), newTVarIO, modifyTVar, atomically, STM, readTVar)
import Control.Concurrent.STM (throwSTM)


new :: MonadIO m => m Session.Pool
new = do
  tvar <- newTVarIO M.empty

  let
      cleanUp = do
        now <- Time.getCurrentTime
        atomically do
          modifyTVar tvar (M.filter (\session -> now > session.expireDate))

  gc <- liftIO $ Timer.repeatedTimer cleanUp (Suspend.sDelay 10)
  pure $ Session.Pool tvar gc


newSession :: Filehub Session
newSession = do
  Session.Pool pool _ <- asks (.sessionPool)
  session             <- Session.createSession
  atomically do
    modifyTVar pool (M.insert session.sessionId session)
  pure session


extendSession :: SessionId -> Filehub ()
extendSession sessionId = do
  duration            <- asks (.sessionDuration)
  Session.Pool pool _ <- asks (.sessionPool)
  now                 <- liftIO Time.getCurrentTime

  let up = maybe Nothing \session -> Just session { expireDate = duration `addUTCTime` now }

  atomically do
    modifyTVar pool (M.alter up sessionId)


delete :: SessionId -> Filehub ()
delete sessionId = do
  Session.Pool pool _ <- asks (.sessionPool)
  atomically do
    modifyTVar pool (M.delete sessionId)


-- | Get a session.
-- This function returns a `Session` directly, if it fails, we throw an Filehub
-- error.
--
-- The reason we don't return an Either or Maybe is because `get` is expected
-- to succeed in almost all call sites. The only place that needs to handle
-- `InvalidSession` is in the wai middleware. Once we pass the middleware
-- check, a session with sessionId should alway exist. If not, it's an
-- unrecoverable exception and there's not much to do about it.
get :: SessionId -> Filehub (STM Session)
get sessionId = do
  Session.Pool pool _ <- asks (.sessionPool)
  pure do
    m <- readTVar pool
    case M.lookup sessionId m of
      Just session -> pure session
      Nothing -> do
        throwSTM (FilehubError InvalidSession "Invalid session")


withSession :: SessionId -> (Session -> STM (Session, a)) -> Filehub a
withSession sessionId f = do
  Session.Pool pool _ <- asks (.sessionPool)
  getSTM <- get sessionId
  atomically do
    s <- getSTM
    (s1, o) <- f s
    let up = maybe Nothing \_ -> Just s1
    modifyTVar pool (M.alter up sessionId)
    pure o


update :: SessionId -> (Session -> Session) -> Filehub (STM ())
update sessionId f = do
  Session.Pool pool _ <- asks (.sessionPool)
  let up = maybe Nothing \session -> Just (f session)

  pure do
    modifyTVar pool (M.alter up sessionId)
