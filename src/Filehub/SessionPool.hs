module Filehub.SessionPool
  ( SessionPool(..)
  , new
  , newSession
  , extendSession
  , deleteSession
  , getSession
  , updateSession
  )
  where

import Effectful.Reader.Dynamic (Reader)
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
import Filehub.Types (Session(..), SessionPool (..), Env, SessionId)
import Filehub.Session qualified as Session
import Filehub.Env.Internal qualified as Env
import Filehub.Error (FilehubError (..))


new :: (IOE :> es) => Eff es SessionPool
new = do
  table <- liftIO HashTable.new
  let cleanUp = do
        flip HashTable.mapM_ table $ \(k, session) -> do
          now <- Time.getCurrentTime
          when (now > session.expireDate) do
            HashTable.delete table k
  gc <- liftIO $ Timer.repeatedTimer cleanUp (Suspend.sDelay 10)
  pure $ SessionPool table gc


newSession :: (Reader Env :> es, IOE :> es) => Eff es Session
newSession = do
  SessionPool pool _ <- Env.getSessionPool
  session <- Session.createSession
  liftIO $ HashTable.insert pool session.sessionId session
  pure session


extendSession :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
extendSession sessionId = do
  duration <- Env.getSessionDuration
  SessionPool pool _ <- Env.getSessionPool
  now <- liftIO Time.getCurrentTime
  liftIO $ HashTable.mutate pool sessionId
    (\case
        Just session -> (Just $ session { expireDate = duration `addUTCTime` now }, ())
        Nothing -> (Nothing, ())
    )


deleteSession :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
deleteSession sessionId = do
  SessionPool pool _ <- Env.getSessionPool
  liftIO $ HashTable.delete pool sessionId


getSession :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es Session
getSession sessionId = do
  SessionPool pool _ <- Env.getSessionPool
  mResult <- liftIO $ HashTable.lookup pool sessionId
  case mResult of
    Just session -> pure session
    Nothing -> do
      logTrace_ [i|No such session #{sessionId}|]
      throwError InvalidSession


updateSession :: (Reader Env :> es, IOE :> es) => SessionId -> (Session -> Session) -> Eff es ()
updateSession sessionId update = do
  SessionPool pool _ <- Env.getSessionPool
  liftIO $ HashTable.mutate pool sessionId
    (\case
        Just session -> (Just $ update session, ())
        Nothing -> (Nothing, ())
    )
