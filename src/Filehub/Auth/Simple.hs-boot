module Filehub.Auth.Simple where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)


newtype Username = Username Text
newtype PasswordHash = PasswordHash ByteString
newtype SimpleAuthUserDB = SimpleAuthUserDB (Map Username PasswordHash)

data UserRecord
instance Eq UserRecord
