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

import Effectful.Reader.Dynamic (Reader, asks)
import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Log (logTrace_, Log)
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


new :: (IOE :> es) => Eff es Session.Pool
new = do
  table <- liftIO HashTable.new
  let cleanUp = do
        flip HashTable.mapM_ table $ \(k, session) -> do
          now <- Time.getCurrentTime
          when (now > session.expireDate) do
            HashTable.delete table k
  gc <- liftIO $ Timer.repeatedTimer cleanUp (Suspend.sDelay 10)
  pure $ Session.Pool table gc


newSession :: (Reader Env :> es, IOE :> es) => Eff es Session
newSession = do
  Session.Pool pool _ <- asks @Env (.sessionPool)
  session <- Session.createSession
  liftIO $ HashTable.insert pool session.sessionId session
  pure session


extendSession :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
extendSession sessionId = do
  duration <- asks @Env (.sessionDuration)
  Session.Pool pool _ <- asks @Env (.sessionPool)
  now <- liftIO Time.getCurrentTime
  liftIO
    $ HashTable.mutate pool sessionId
    $ maybe
        (Nothing, ())
        (\session -> (Just $ session { expireDate = duration `addUTCTime` now }, ()))


delete :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
delete sessionId = do
  Session.Pool pool _ <- asks @Env (.sessionPool)
  liftIO $ HashTable.delete pool sessionId


get :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es Session
get sessionId = do
  Session.Pool pool _ <- asks @Env (.sessionPool)
  mResult <- liftIO $ HashTable.lookup pool sessionId
  case mResult of
    Just session -> pure session
    Nothing -> do
      logTrace_ [i|No such session #{sessionId}|]
      throwError (FilehubError InvalidSession "Invalid session")


update :: (Reader Env :> es, IOE :> es) => SessionId -> (Session -> Session) -> Eff es ()
update sessionId f = do
  Session.Pool pool _ <- asks @Env (.sessionPool)
  liftIO
    $ HashTable.mutate pool sessionId
    $ maybe
        (Nothing, ())
        (\session -> (Just $ f session, ()))
