module Filehub.Session.Pool
  ( Session.Pool(..)
  , new
  , newSession
  , extendSession
  , delete
  , get
  , withSession
  , withSession_
  , modifySession
  )
  where

import Filehub.Prelude
import Control.Concurrent.STM (throwSTM)
import Control.Concurrent.Suspend qualified as Suspend
import Control.Concurrent.Timer qualified as Timer
import Control.Monad ((>=>))
import Control.Monad.Reader (asks)
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Monad (Filehub)
import Filehub.Session.Internal qualified as Session
import Filehub.Session.Types (Session(..), SessionId)
import Filehub.Session.Types qualified as Session
import Filehub.Types (Env(..))
import UnliftIO (newTVarIO, modifyTVar, atomically, STM, readTVar)


new :: MonadIO m => m Session.Pool
new = do
  tvar <- newTVarIO map_empty

  let
      cleanUp = do
        now <- time_now
        atomically do
          modifyTVar tvar (map_filter (\session -> now <= session.expireDate))

  gc <- liftIO $ Timer.repeatedTimer cleanUp (Suspend.sDelay 60)
  pure $ Session.Pool tvar gc


newSession :: Filehub Session
newSession = do
  Session.Pool pool _ <- asks (.sessionPool)
  session             <- Session.createSession
  atomically do
    modifyTVar pool (map_insert session.sessionId session)
  pure session


extendSession :: SessionId -> Filehub ()
extendSession sessionId = do
  duration            <- asks (.sessionDuration)
  Session.Pool pool _ <- asks (.sessionPool)
  now                 <- liftIO time_now

  let up = maybe Nothing \session -> Just session { expireDate = duration `time_add` now }

  atomically do
    modifyTVar pool (map_alter up sessionId)


delete :: SessionId -> Filehub ()
delete sessionId = do
  Session.Pool pool _ <- asks (.sessionPool)
  atomically do
    modifyTVar pool (map_delete sessionId)


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
    case map_lookup sessionId m of
      Just session -> pure session
      Nothing -> do
        throwSTM (FilehubError InvalidSession "Invalid session")


withSession_ :: SessionId -> (Session -> STM (Session, a)) -> Filehub a
withSession_ sessionId f = do
  Session.Pool pool _ <- asks (.sessionPool)
  getSTM <- get sessionId
  atomically do
    s <- getSTM
    (s1, o) <- f s
    let up = maybe Nothing \_ -> Just s1
    modifyTVar pool (map_alter up sessionId)
    pure o


withSession :: SessionId -> (Session -> STM a) -> Filehub a
withSession sessionId f = withSession_ sessionId (\s -> f s >>= \a -> pure (s, a))


modifySession :: SessionId -> (Session -> STM Session) -> Filehub ()
modifySession sessionId f = withSession_ sessionId (f >=> \s' -> pure (s', ()))
