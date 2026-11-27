module Filehub.ActiveUser.Types (ActiveUser(..), Pool(..)) where

import Filehub.Prelude
import Prelude hiding (readFile)
import Filehub.Session.Types.SessionId (SessionId)
import Filehub.Auth.Types.AuthId (AuthId(..))
import Filehub.Auth.Types (Auth)
import Data.HashTable.IO (BasicHashTable)


data ActiveUser = ActiveUser
  { authId   :: AuthId
  , loginAt  :: UTCTime
  , sessions :: [SessionId]
  , auth     :: Auth
  }


data Pool = Pool
  { pool :: BasicHashTable AuthId ActiveUser
  }
