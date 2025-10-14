module Filehub.Auth.Simple
  ( UserRecord(..)
  , Username(..)
  , PasswordHash(..)
  , SimpleAuthUserDB(..)
  , validate
  , createSimpleAuthUserDB
  , authenticateSession
  , createActiveUser
  )
  where

import Control.Monad (forM)
import Crypto.BCrypt qualified as BCrypt
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time qualified as Time
import Effectful (Eff, (:>), MonadIO (..), IOE)
import Effectful.Reader.Dynamic (asks)
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.ActiveUser.Types (ActiveUser (..))
import Filehub.Auth.Types (createAuthId, AuthId, Auth (..))
import Filehub.Env (Env(..))
import Filehub.Monad (Filehub)
import Filehub.Session (SessionId, Session)
import Filehub.Session qualified as Session
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Types (LoginForm (..))
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


validate :: Username -> ByteString -> SimpleAuthUserDB -> Bool
validate name password (SimpleAuthUserDB db) =
  case Map.lookup name db of
    Just (PasswordHash hash) -> BCrypt.validatePassword hash password
    Nothing                  -> False


createSimpleAuthUserDB :: (IOE :> es) => [UserRecord] -> Eff es SimpleAuthUserDB
createSimpleAuthUserDB loginInfo =
  case loginInfo of
    [] -> pure (SimpleAuthUserDB mempty)
    infos -> fromList infos
  where
    fromList xs = liftIO $ do
      infos <- forM xs \(UserRecord u p) -> do
        let username = Username (Text.pack u)
        mHash <- BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy (Char8.pack p)
        pure $ maybeToList (fmap (\hash -> (username, PasswordHash hash)) mHash)
      pure
        . SimpleAuthUserDB
        . Map.fromList
        . mconcat
        $ infos


-- | Handle the simple authetication login.
authenticateSession :: SessionId -> LoginForm -> Filehub (Maybe Session)
authenticateSession sessionId (LoginForm username password) =  do
  db <- asks @Env (.simpleAuthUserDB)
  let username' =  Username username
  if (validate username' (Text.encodeUtf8 password) db) then do
    authId <- createAuthId
    Session.setAuthId sessionId (Just authId)
    activeUser <- createActiveUser authId sessionId username'
    ActiveUser.Pool.add activeUser
    Just <$> Session.Pool.get sessionId
  else pure Nothing


createActiveUser :: (IOE :> es) => AuthId -> SessionId -> Username -> Eff es ActiveUser
createActiveUser authId sessionId username = do
  now <- liftIO Time.getCurrentTime
  pure ActiveUser
    { authId   = authId
    , loginAt  = now
    , sessions = [sessionId]
    , auth     = Simple username
    }
