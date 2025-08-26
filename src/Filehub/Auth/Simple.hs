{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Auth.Simple where

import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Hashable (Hashable)
import Crypto.BCrypt qualified as BCrypt
import Effectful (Eff, (:>), MonadIO (..), IOE)
import Control.Monad (forM)
import Data.Maybe (maybeToList)
import Prelude hiding (readFile)


newtype Username = Username Text deriving (Show, Eq, Ord, Hashable)
newtype PasswordHash = PasswordHash ByteString deriving (Show, Eq, Ord)
newtype SimpleAuthUserDB = SimpleAuthUserDB (Map Username PasswordHash) deriving (Show, Eq)


data LoginUser = LoginUser
  { username :: String
  , password :: String
  }
  deriving (Show, Eq)


validate :: Username -> ByteString -> SimpleAuthUserDB -> Bool
validate name password (SimpleAuthUserDB db) =
  case Map.lookup name db of
    Just (PasswordHash hash) -> BCrypt.validatePassword hash password
    Nothing -> False


createSimpleAuthUserDB :: (IOE :> es) => [LoginUser] -> Eff es SimpleAuthUserDB
createSimpleAuthUserDB loginInfo =
  case loginInfo of
    [] -> pure (SimpleAuthUserDB mempty)
    infos -> fromList infos
  where
    fromList xs = liftIO $ do
      infos <- forM xs $ \(LoginUser u p) -> do
        let username = Username . Text.pack $ u
        mHash <- BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy (Char8.pack p)
        pure $ maybeToList $ fmap (\hash -> (username, PasswordHash hash)) mHash
      pure $ SimpleAuthUserDB . Map.fromList . mconcat $ infos
