{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Auth.Simple where

import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Hashable (Hashable)
import Crypto.BCrypt qualified as BCrypt
import Effectful (Eff, (:>), MonadIO (..), IOE)
import Filehub.Config (LoginUser(..))
import Control.Monad (forM)
import Data.Maybe (maybeToList)
import Prelude hiding (readFile)


newtype AuthId = AuthId UUID deriving (Show, Eq, Ord, Hashable)
newtype Username = Username Text deriving (Show, Eq, Ord, Hashable)
newtype PasswordHash = PasswordHash ByteString deriving (Show, Eq, Ord)
newtype UserDB = UserDB (Map Username PasswordHash) deriving (Show, Eq)


createAuthId :: (IOE :> es) => Eff es AuthId
createAuthId = AuthId <$> liftIO UUID.nextRandom


validate :: Username -> ByteString -> UserDB -> Bool
validate name password (UserDB db) =
  case Map.lookup name db of
    Just (PasswordHash hash) -> BCrypt.validatePassword hash password
    Nothing -> False


createUserDB :: (IOE :> es) => [LoginUser] -> Eff es UserDB
createUserDB loginInfo =
  case loginInfo of
    [] -> pure (UserDB mempty)
    infos -> fromList infos
  where
    fromList xs = liftIO $ do
      infos <- forM xs $ \(LoginUser u p) -> do
        let username = Username . Text.pack $ u
        mHash <- BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy (Char8.pack p)
        pure $ maybeToList $ fmap (\hash -> (username, PasswordHash hash)) mHash
      pure $ UserDB . Map.fromList . mconcat $ infos
