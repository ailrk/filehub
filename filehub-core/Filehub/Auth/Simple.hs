module Filehub.Auth.Simple
  ( validate
  , createSimpleAuthUserDB
  , authenticateSession
  , createActiveUser
  )
  where

import Filehub.Prelude
import Crypto.BCrypt qualified as BCrypt
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.ActiveUser.Types (ActiveUser (..))
import Filehub.Auth.Types (createAuthId, AuthId, Auth (..))
import Filehub.Env (Env(..))
import Filehub.Session (SessionId, Session)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Types (LoginForm (..))
import Prelude hiding (readFile)
import Filehub.Monad (Filehub)
import Control.Monad.Reader (asks)
import Filehub.Auth.Types.Simple
import Filehub.Session.Pool (modifySession)
import Filehub.Session (Session(..))


validate :: Username -> ByteString -> SimpleAuthUserDB -> Bool
validate name password (SimpleAuthUserDB db) =
  case map_lookup name db of
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
        let username = Username (txt_pack u)
        mHash <- BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy (bs_packChars p)
        pure $ maybeToList (fmap (\hash -> (username, PasswordHash hash)) mHash)
      pure
        . SimpleAuthUserDB
        . map_fromList
        . mconcat
        $ infos


-- | Handle the simple authetication login.
authenticateSession :: SessionId -> LoginForm -> Filehub (Maybe Session)
authenticateSession sessionId (LoginForm username password) = do
  db <- asks (.simpleAuthUserDB)
  let username' =  Username username
  if (validate username' (txt_encode password) db) then do
    authId <- createAuthId
    modifySession sessionId \s -> pure $ s { authId = (Just authId) }
    activeUser <- createActiveUser authId sessionId username'
    ActiveUser.Pool.add activeUser
    Just <$> (Session.Pool.get sessionId >>= atomically)
  else pure Nothing


createActiveUser :: AuthId -> SessionId -> Username -> Filehub ActiveUser
createActiveUser authId sessionId username = do
  now <- liftIO time_now
  pure ActiveUser
    { authId   = authId
    , loginAt  = now
    , sessions = [sessionId]
    , auth     = Simple username
    }
