module Filehub.SessionPool where

import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Data.HashTable.IO qualified as HashTable
import Filehub.Types (Session(..), SessionPool (..), Env, SessionId)
import Filehub.Session qualified as Session
import {-# SOURCE #-} Filehub.Env qualified as Env
import Data.Time (NominalDiffTime, addUTCTime)


empty :: IOE :> es => Eff es SessionPool
empty = SessionPool <$>  liftIO HashTable.new


newSession :: (Reader Env :> es, IOE :> es) => Eff es Session
newSession = do
  SessionPool pool <- Env.getSessionPool
  session <- Session.createSession
  liftIO $ HashTable.insert pool session.sessionId session
  pure session


extendSession :: (Reader Env :> es, IOE :> es) => SessionId -> NominalDiffTime -> Eff es ()
extendSession sessionId duration = do
  SessionPool pool <- Env.getSessionPool
  liftIO $ HashTable.mutate pool sessionId
    (\case
        Just session -> (Just $ session { expireDate = duration `addUTCTime` session.expireDate }, ())
        Nothing -> (Nothing, ())
    )


deleteSession :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
deleteSession sessionId = do
  SessionPool pool <- Env.getSessionPool
  liftIO $ HashTable.delete pool sessionId


getSession :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es (Maybe Session)
getSession sessionId = do
  SessionPool pool <- Env.getSessionPool
  liftIO $ HashTable.lookup pool sessionId


updateSession :: (Reader Env :> es, IOE :> es) => SessionId -> (Session -> Session) -> Eff es ()
updateSession sessionId update = do
  SessionPool pool <- Env.getSessionPool
  liftIO $ HashTable.mutate pool sessionId
    (\case
        Just session -> (Just $ update session, ())
        Nothing -> (Nothing, ())
    )
