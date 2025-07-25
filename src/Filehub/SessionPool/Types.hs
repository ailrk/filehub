module Filehub.SessionPool.Types where

import Control.Concurrent.Timer qualified as Timer
import Data.HashTable.IO (BasicHashTable)
import Filehub.Session.Types (SessionId, Session)


data SessionPool = SessionPool
  { pool :: BasicHashTable SessionId Session
  , gc :: Timer.TimerIO
  -- ^ garbage collector, periodically clean up expired sessions.
  }
