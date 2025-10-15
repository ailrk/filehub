{-# LANGUAGE NamedFieldPuns #-}
module EvtLog
  ( Handle
  , LogEvt(..)
  , ToLogEvt(..)
  , FromLogEvt(..)
  , emit
  , fold
  , initialize
  , close
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
import Control.Monad (when)


data Handle = Handle
  { conn    :: Connection
  , maxSize :: Int
  }


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
    ( id    INTEGER PRIMARY KEY AUTOINCREMENT
    , time  INTEGER NOT NULL
    , type  TEXT NOT NULL
    , data  TEXT NOT NULL
    , UNIQUE(id)
    )
  |]


emit :: (ToLogEvt evt, MonadIO m) => Handle -> evt -> m ()
emit Handle { conn, maxSize } evt = liftIO do
  Sqlite.withTransaction conn do
    countResult <- Sqlite.query_ @(Only Int) conn [iii|SELECT COUNT(*) FROM evtlog |]
    case countResult of
      [Only n] -> do
        when (n >= maxSize) do
          Sqlite.execute conn [iii|
            DELETE FROM evtlog
            WHERE id IN
              (SELECT id FROM evtlog ORDER BY time ASC LIMIT ?)
          |] (Only toDelete)
      _ -> error "impossible"
    Sqlite.execute conn [i| INSERT INTO evtlog (time, type, data) VALUES (?,?,?) |] logEvt
  where
    toDelete = floor @Double @Int (0.2 * fromIntegral maxSize)
    logEvt = toLogEvt evt


fold :: (FromLogEvt evt, MonadIO m)
     => Handle
     -> Maybe (UTCTime, UTCTime)
     -> (proj -> evt -> proj) -> proj
     -> m proj
fold Handle { conn } mRange f proj = liftIO go
  where
    go = case mRange of
           Just (from, to) -> do
             Sqlite.fold conn
               [iii|
                 SELECT time, type, data FROM evtlog
                   WHERE time >= ? AND time <= ?
               |]
               (Only from :. Only to) proj
               step

           Nothing -> do
             Sqlite.fold_ conn
               [i| SELECT time, type, data from evtlog |]
               proj
               step

    step p evt = do
      case fromLogEvt evt of
        Just e -> pure (f p e)
        Nothing -> do
          throwIO (userError ("failed to decode log event: " <> show evt))


initialize :: MonadIO m => String -> Int -> m Handle
initialize connectionStr maxSize = liftIO do
  conn <- Sqlite.open connectionStr
  Sqlite.execute_ conn [i| PRAGMA foreign_keys = ON; |]
  Sqlite.execute_ conn schema
  pure Handle
    { conn    = conn
    , maxSize = maxSize
    }


close :: MonadIO m => Handle -> m ()
close Handle { conn } = liftIO do
  Sqlite.close conn
