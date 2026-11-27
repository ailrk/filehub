module Filehub.Auth.Types.Simple where

import Filehub.Prelude
import Data.Hashable (Hashable)
import Prelude hiding (readFile)
import Text.Debug (Debug(..))


newtype Username = Username Text
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable)


newtype PasswordHash = PasswordHash ByteString deriving (Show, Eq, Ord)


newtype SimpleAuthUserDB = SimpleAuthUserDB (Map Username PasswordHash) deriving (Show, Eq)


-- | A single user record
data UserRecord = UserRecord
  { username :: String
  , password :: String
  }
  deriving (Show, Eq)


instance Debug Username where         debug = show
instance Debug PasswordHash where     debug = show
instance Debug SimpleAuthUserDB where debug = show
instance Debug UserRecord where       debug = show
