{-# LANGUAGE NamedFieldPuns #-}
module EvtLog
  ( LogEvt(..)
  , ToLogEvt(..)
  , FromLogEvt(..)
  , emit
  , fold
  , connect
  )
  where

import Data.ByteString (ByteString)
import Data.String.Interpolate (iii, i)
import Database.SQLite.Simple (Connection, Query, Only (..), type (:.) (..))
import UnliftIO (MonadIO, liftIO, throwIO)
import Database.SQLite.Simple qualified as Sqlite
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromRow
import Data.Time (UTCTime)
import Data.ByteString.Lazy qualified as LBS


data LogEvt = LogEvt
  { eventTime :: UTCTime
  , eventType :: ByteString
  , eventData :: LBS.ByteString
  }
  deriving (Show)


instance FromRow LogEvt where
  fromRow = LogEvt <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field


instance ToRow LogEvt where
  toRow LogEvt { eventTime, eventType, eventData } = toRow (eventTime, eventType, eventData)


class ToLogEvt a where
  toLogEvt :: a -> LogEvt


class FromLogEvt a where
  fromLogEvt :: LogEvt -> Maybe a


schema :: Query
schema = [iii|
    CREATE TABLE IF NOT EXISTS evtlog
    ( id    INTEGER KEY NOT NULL
    , time  INTEGER NOT NULL
    , type  TEXT NOT NULL
    , data  TEXT NOT NULL
    , UNIQUE(id)
    )
  |]


emit :: (ToLogEvt evt, MonadIO m) => Connection -> evt -> m ()
emit conn evt = liftIO do
  Sqlite.execute conn [iii|
       INSERT INTO entry (id, time, type, data) VALUES (?,?,?,?)
        ON CONFLICT(id, todo_date) DO UPDATE SET
        description = excluded.description,
        completed   = excluded.completed;
    |] logEvt
  where
    logEvt = toLogEvt evt


fold :: (FromLogEvt evt, MonadIO m)
     => Connection
     -> Maybe (UTCTime, UTCTime)
     -> (proj -> evt -> proj) -> proj
     -> m proj
fold conn mRange f proj = liftIO go
  where
    go = case mRange of
           Just (from, to) -> do
             Sqlite.fold conn
               [iii|
                 SELECT (id, time, type, data) FROM evtlog
                   WHERE time >= ? AND time <= ?
               |]
               (Only from :. Only to) proj
               step

           Nothing -> do
             Sqlite.fold_ conn
               [i| SELECT (id, time, type, data) from evtlog |]
               proj
               step

    step p evt = do
      case fromLogEvt evt of
        Just e -> pure (f p e)
        Nothing -> do
          throwIO (userError ("failed to decode log event: " <> show evt))


connect :: MonadIO m => String -> m Connection
connect connectionStr = liftIO do
  conn <- Sqlite.open connectionStr
  Sqlite.execute_ conn [i| PRAGMA foreign_keys = ON; |]
  Sqlite.execute_ conn schema
  pure conn
