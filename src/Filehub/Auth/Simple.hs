module Filehub.Auth.Simple
  ( validate
  , createSimpleAuthUserDB
  , authenticateSession
  , createActiveUser
  )
  where

import Control.Monad (forM)
import Crypto.BCrypt qualified as BCrypt
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time qualified as Time
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.ActiveUser.Types (ActiveUser (..))
import Filehub.Auth.Types (createAuthId, AuthId, Auth (..))
import Filehub.Env (Env(..))
import Filehub.Session (SessionId, Session)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Types (LoginForm (..))
import Prelude hiding (readFile)
import Filehub.Session qualified as Session
import Filehub.Monad (Filehub)
import Control.Monad.Reader (asks)
import UnliftIO (MonadIO(..), atomically)
import Filehub.Auth.Types.Simple


validate :: Username -> ByteString -> SimpleAuthUserDB -> Bool
validate name password (SimpleAuthUserDB db) =
  case Map.lookup name db of
    Just (PasswordHash hash) -> BCrypt.validatePassword hash password
    Nothing                  -> False


createSimpleAuthUserDB :: MonadIO m => [UserRecord] -> m SimpleAuthUserDB
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
authenticateSession sessionId (LoginForm username password) = do
  db <- asks (.simpleAuthUserDB)
  let username' =  Username username
  if (validate username' (Text.encodeUtf8 password) db) then do
    authId <- createAuthId
    Session.set sessionId (.authId) (Just authId)
    activeUser <- createActiveUser authId sessionId username'
    ActiveUser.Pool.add activeUser
    Just <$> (Session.Pool.get sessionId >>= atomically)
  else pure Nothing


createActiveUser :: AuthId -> SessionId -> Username -> Filehub ActiveUser
createActiveUser authId sessionId username = do
  now <- liftIO Time.getCurrentTime
  pure ActiveUser
    { authId   = authId
    , loginAt  = now
    , sessions = [sessionId]
    , auth     = Simple username
    }
