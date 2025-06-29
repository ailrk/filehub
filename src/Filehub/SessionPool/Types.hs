module Filehub.SessionPool.Types where

import Data.HashTable.IO (BasicHashTable)
import Filehub.Session.Types
import Control.Concurrent.Timer qualified as Timer

data SessionPool = SessionPool
  { pool :: BasicHashTable SessionId Session
  , gc :: Timer.TimerIO
  -- ^ garbage collector, periodically clean up expired sessions.
  }
