-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Global in-memory operation history (capped at 500 entries).
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Filehub.History
  ( History
  , HistoryEntry(..)
  , Operation(..)
  , newHistory
  , withHistory
  , recordHistory
  , getHistory
  , lookupEntry
  , describeOperation
  ) where

import Filehub.Prelude
import Control.Monad.Reader (asks)
import Filehub.Env (Env(..))
import Filehub.Error (FilehubError)
import Filehub.History.Types
import Filehub.Monad (Filehub)
import Filehub.Session (SessionId)


recordHistory :: SessionId -> Operation -> Bool -> Filehub ()
recordHistory sessionId op succeeded = do
  h   <- asks (.history)
  now <- liftIO time_now
  eid <- liftIO (nextId h)
  let entry = HistoryEntry { entryId = eid, timestamp = now, sessionId
                           , operation = op, succeeded }
  liftIO $ addEntry h entry


withHistory :: SessionId -> Operation -> Filehub a -> Filehub a
withHistory sessionId op action = do
  result <- try action
  case result of
    Left  (e :: FilehubError) -> recordHistory sessionId op False >> throwIO e
    Right a                   -> recordHistory sessionId op True  >> pure a
