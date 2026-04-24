module Filehub.Session.Pool
  ( Session.Pool(..)
  , new
  , newSession
  , extendSession
  , delete
  , get
  , update
  )
  where

import Data.Time (addUTCTime)
import Data.Time.Clock qualified as Time
import Data.HashTable.IO qualified as HashTable
import Data.String.Interpolate (i)
import Control.Concurrent.Timer qualified as Timer
import Control.Concurrent.Suspend qualified as Suspend
import Control.Monad (when)
import Filehub.Types (Env(..))
import Filehub.Session.Internal qualified as Session
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Session.Types (Session(..), SessionId)
import Filehub.Session.Types qualified as Session
import Filehub.Monad (Filehub)
import Control.Monad.Reader (asks)
import UnliftIO (MonadIO(..), throwIO)
import Log (logTrace_)


new :: MonadIO m => m Session.Pool
new = do
  table <- liftIO HashTable.new
  let cleanUp = do
        flip HashTable.mapM_ table $ \(k, session) -> do
          now <- Time.getCurrentTime
          when (now > session.expireDate) do
            HashTable.delete table k
  gc <- liftIO $ Timer.repeatedTimer cleanUp (Suspend.sDelay 10)
  pure $ Session.Pool table gc


newSession :: Filehub Session
newSession = do
  Session.Pool pool _ <- asks @Env (.sessionPool)
  session             <- Session.createSession
  liftIO $ HashTable.insert pool session.sessionId session
  pure session


extendSession :: SessionId -> Filehub ()
extendSession sessionId = do
  duration            <- asks @Env (.sessionDuration)
  Session.Pool pool _ <- asks @Env (.sessionPool)
  now                 <- liftIO Time.getCurrentTime
  liftIO
    $ HashTable.mutate pool sessionId
    $ maybe
        (Nothing, ())
        \session -> (Just session { expireDate = duration `addUTCTime` now }, ())


delete :: SessionId -> Filehub ()
delete sessionId = do
  Session.Pool pool _ <- asks @Env (.sessionPool)
  liftIO $ HashTable.delete pool sessionId


-- | Get a session.
-- This function returns a `Session` directly, if it fails, we throw an Filehub
-- error.
--
-- The reason we don't return an Either or Maybe is because `get` is expected
-- to succeed in almost all call sites. The only place that needs to handle
-- `InvalidSession` is in the wai middleware. Once we pass the middleware
-- check, a session with sessionId should alway exist. If not, it's an
-- unrecoverable exception and there's not much to do about it.
get :: SessionId -> Filehub Session
get sessionId = do
  Session.Pool pool _ <- asks @Env (.sessionPool)
  mResult <- liftIO $ HashTable.lookup pool sessionId
  case mResult of
    Just session -> pure session
    Nothing -> do
      logTrace_ [i|[zsv09d] No such session #{sessionId}|]
      throwIO (FilehubError InvalidSession "Invalid session")


update :: SessionId -> (Session -> Session) -> Filehub ()
update sessionId f = do
  Session.Pool pool _ <- asks @Env (.sessionPool)
  liftIO
    $ HashTable.mutate pool sessionId
    $ maybe
        (Nothing, ())
        \session -> (Just (f session), ())
