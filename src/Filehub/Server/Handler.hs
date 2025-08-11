module Filehub.Server.Handler
  ( toServantHandler
  , readOnlyHandler
  , sessionHandler
  , desktopOnlyHandler
  , mobileOnlyHandler
  , displayOnlyHandler
  , loginHandler
  , ConfirmDesktopOnly(..)
  , ConfirmMobilOnly(..)
  , ConfirmReadOnly(..)
  , ConfirmLogin(..)
  )
  where


import Data.Bifunctor (Bifunctor(..))
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Filehub.Types (SessionId)
import Filehub.Cookie qualified as Cookie
import Filehub.Env (Env(..))
import Filehub.Error (withServerError)
import Filehub.Session.Types (Session(..))
import Filehub.SessionPool qualified as SessionPool
import Network.Wai
import Prelude hiding (readFile)
import Lens.Micro.Platform ()
import Lens.Micro
import Servant
import Filehub.Types (Display (..))
import Filehub.Env qualified as Env
import Filehub.Cookie qualified as Cookies
import Filehub.Server.Internal (parseHeader')
import Filehub.Monad (runFilehub, Filehub)
import UnliftIO (MonadIO(..))
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Trans.Except (ExceptT(..))
import Network.HTTP.Types.Header (hLocation)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad (guard)


toServantHandler :: Env -> Filehub a -> Handler a
toServantHandler env eff
  = Handler
  . ExceptT
  . runFilehub env
  $ eff


-- | withness
data ConfirmReadOnly = ConfirmReadOnly


readOnlyHandler :: Env -> AuthHandler Request ConfirmReadOnly
readOnlyHandler env =
  mkAuthHandler $ \_ ->
    if env.readOnly
      then throwError err400 { errBody = "Read-only mode is enabled" }
      else return ConfirmReadOnly


sessionHandler :: Env -> AuthHandler Request SessionId
sessionHandler env = mkAuthHandler handler
  where
    toEither msg Nothing = Left msg
    toEither _ (Just x) = Right x

    throw401 msg = throwError $ err401 { errBody = msg }

    handler :: Request -> Handler SessionId
    handler req = do
      sessionId <- either throw401 pure do
        header <- toEither "cookie not found" $ lookup "Cookie" $ requestHeaders req
        cookie <- bimap (Text.encodeUtf8 . Text.fromStrict) id $ parseHeader header
        toEither "can't get sessionId" $ Cookie.getSessionId cookie
      _ <- toServantHandler env $ SessionPool.getSession sessionId & withServerError
      pure sessionId


-- | Withness
data ConfirmDesktopOnly = ConfirmDesktopOnly
data ConfirmMobilOnly = ConfirmMobilOnly


desktopOnlyHandler :: Env -> AuthHandler Request ConfirmDesktopOnly
desktopOnlyHandler =
  displayOnlyHandler
    ConfirmDesktopOnly
    (\case
      Desktop -> True
      _ -> False
    )
    "Only allowed for desktop view"


mobileOnlyHandler :: Env -> AuthHandler Request ConfirmMobilOnly
mobileOnlyHandler =
  displayOnlyHandler
    ConfirmMobilOnly
    (\case
      Mobile -> True
      _ -> False
    )
    "Only allowed for mobile view"


displayOnlyHandler :: witness -> (Display -> Bool) -> ByteString -> Env -> AuthHandler Request witness
displayOnlyHandler witness predicate msg env =
  mkAuthHandler $ \req -> do
    sessionId <- maybe (throwError $ err401 { errBody = "invalid session" }) pure do
      cookie <-  lookup "Cookie" $ requestHeaders req
      parseHeader' cookie >>= Cookies.getSessionId
    display <- liftIO . runFilehub env $ Env.getDisplay sessionId & withServerError
    case display of
      Right d | predicate d -> pure witness
      _ -> throwError err400 { errBody = msg }


data ConfirmLogin = ConfirmLogin


-- | If we noLogin is True we simply by pass this auth check
--   otherwise we check the validity of the current auth Id from cookie.
loginHandler :: Env -> AuthHandler Request ConfirmLogin
loginHandler env = mkAuthHandler $ \req -> do
    result <- runMaybeT do
      cookie <- MaybeT . pure $ lookup "Cookie" (requestHeaders req)
      sessionId <- MaybeT . pure $ parseHeader' cookie >>= Cookies.getSessionId
      let authId = parseHeader' cookie >>= Cookies.getAuthId
      eSession  <- liftIO . runFilehub env . withServerError $ Env.getSession sessionId
      session   <- MaybeT . pure $ either (const Nothing) Just eSession
      guard (env.noLogin || session.authId == authId)
      pure ConfirmLogin
    maybe redirect pure result
  where
    redirect = throwError (err307 { errHeaders = [(hLocation, "/login")] })
