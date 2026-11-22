module Filehub.Handler
  ( toServantHandler
  , readOnlyHandler
  , sessionHandler
  , desktopOnlyHandler
  , mobileOnlyHandler
  , displayOnlyHandler
  , loginHandler
  , sharedLinkPermitHandler
  , rateLimitLoginHandler
  , rateLimitUploadHandler
  , rateLimitDownloadHandler
  , rateLimitSearchHandler
  , rateLimitSharedLinkHandler
  , ConfirmDesktopOnly(..)
  , ConfirmMobilOnly(..)
  , ConfirmReadOnly(..)
  , ConfirmLogin(..)
  , ConfirmRateLimitLogin(..)
  , ConfirmRateLimitUpload(..)
  , ConfirmRateLimitDownload(..)
  , ConfirmRateLimitSearch(..)
  , ConfirmRateLimitSharedLink(..)
  , clientIP
  )
  where

import Data.Bifunctor (Bifunctor(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Filehub.RateLimit qualified as RateLimit
import Filehub.RateLimit (RateLimiter)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Filehub.Types (SessionId, Display(..))
import Filehub.Cookie qualified as Cookie
import Filehub.Env (Env(..))
import Filehub.Env qualified as Env
import Filehub.Error (toServerError)
import Network.Wai
import Prelude hiding (readFile)
import Servant
import Filehub.Cookie qualified as Cookies
import Filehub.Monad (runFilehub, Filehub)
import UnliftIO (MonadIO(..), atomically)
import Data.ByteString.Lazy qualified as LBS
import Control.Monad.Trans.Except (ExceptT(..), withExceptT)
import Network.HTTP.Types.Header (hLocation)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad (guard)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Cookie (FromCookies(..))
import Filehub.SharedLink (SharedLinkPermit)
import Filehub.Session qualified as Session
import Filehub.Session.Pool (withSession)
import Filehub.Session.Handle (getDisplay)
import Network.HTTP.Headers.Extended (parseHeader')


toServantHandler :: Env -> Filehub a -> Handler a
toServantHandler env eff
  = Handler
  . withExceptT toServerError
  . ExceptT
  . runFilehub env
  $ eff


-- | Withness
data ConfirmReadOnly = ConfirmReadOnly


readOnlyHandler :: Env -> AuthHandler Request ConfirmReadOnly
readOnlyHandler env =
  mkAuthHandler \_ ->
    if env.readOnly
      then throwError err400 { errBody = "Read-only mode is enabled" }
      else return ConfirmReadOnly


sessionHandler :: Env -> AuthHandler Request SessionId
sessionHandler env = mkAuthHandler handler
  where
    toEither msg Nothing = Left msg
    toEither _ (Just x) = Right x

    throw401 msg = throwError err401 { errBody = msg }

    handler :: Request -> Handler SessionId
    handler req = do
      sessionId <- either throw401 pure do
        header <- toEither "cookie not found" $ lookup "Cookie" (requestHeaders req)
        cookie <- first (TL.encodeUtf8 . TL.fromStrict) $ parseHeader header
        toEither "can't get sessionId" (Cookie.fromCookies cookie)
      _ <- toServantHandler env (Session.Pool.get sessionId)
      pure sessionId


-- | Withness
data ConfirmDesktopOnly = ConfirmDesktopOnly
data ConfirmMobilOnly   = ConfirmMobilOnly


desktopOnlyHandler :: Env -> AuthHandler Request ConfirmDesktopOnly
desktopOnlyHandler =
  displayOnlyHandler
    ConfirmDesktopOnly
    (\case
      Desktop -> True
      _       -> False)
    "Only allowed for desktop view"


mobileOnlyHandler :: Env -> AuthHandler Request ConfirmMobilOnly
mobileOnlyHandler =
  displayOnlyHandler
    ConfirmMobilOnly
    (\case
      Mobile -> True
      _      -> False)
    "Only allowed for mobile view"


displayOnlyHandler :: witness -> (Display -> Bool) -> LBS.ByteString -> Env -> AuthHandler Request witness
displayOnlyHandler witness predicate msg env =
  mkAuthHandler \req -> do
    sessionId <- maybe (throwError err401 { errBody = "invalid session" }) pure do
      cookie <-  lookup "Cookie" (requestHeaders req)
      parseHeader' cookie >>= Cookies.fromCookies @SessionId
    display <- liftIO $ runFilehub env do
      withSession sessionId getDisplay
    case display of
      Right d | predicate d -> pure witness
      _                     -> throwError err400 { errBody = msg }


data ConfirmLogin = ConfirmLogin


-- | If we noLogin is True we simply by pass this auth check
--   otherwise we check the validity of the current auth Id from cookie.
loginHandler :: Env -> AuthHandler Request ConfirmLogin
loginHandler env
  | Env.hasNoLogin env = mkAuthHandler \_ -> pure ConfirmLogin
  | otherwise = mkAuthHandler \req -> do
      result <- runMaybeT do
        cookie    <- MaybeT . pure $ lookup "Cookie" (requestHeaders req)
        sessionId <- MaybeT . pure $ parseHeader' cookie >>= fromCookies
        authId    <- MaybeT . pure $ parseHeader' cookie >>= fromCookies
        eSession  <- liftIO $ runFilehub env (Session.Pool.get sessionId >>= atomically)
        session   <- MaybeT . pure $ either (const Nothing) Just eSession
        guard (session.authId == Just authId)
        pure ConfirmLogin
      maybe redirect pure result
  where
    redirect = throwError (err307 { errHeaders = [(hLocation, "/login")] })


sharedLinkPermitHandler :: Env -> AuthHandler Request (Maybe SharedLinkPermit)
sharedLinkPermitHandler _env = mkAuthHandler \req ->
  pure $ lookup "Cookie" (requestHeaders req) >>= parseHeader' >>= fromCookies @SharedLinkPermit


-- ---------------------------------------------------------------------------
-- Rate-limit guards
-- Each corresponds to one AuthProtect tag in Routes.hs.
-- Distinct types are required so Servant's Context lookup stays unambiguous.
-- ---------------------------------------------------------------------------

data ConfirmRateLimitLogin      = ConfirmRateLimitLogin
data ConfirmRateLimitUpload     = ConfirmRateLimitUpload
data ConfirmRateLimitDownload   = ConfirmRateLimitDownload
data ConfirmRateLimitSearch     = ConfirmRateLimitSearch
data ConfirmRateLimitSharedLink = ConfirmRateLimitSharedLink


-- | Extract client IP, honouring X-Forwarded-For for reverse-proxy
-- deployments.  Strips the port from the WAI remoteHost fallback.
-- IPv4 show format: "1.2.3.4:5678"  IPv6: "[::1]:5678"
clientIP :: Request -> Text
clientIP req =
  case lookup "X-Forwarded-For" (requestHeaders req) of
    Just xff -> T.strip . T.takeWhile (/= ',') $ T.decodeUtf8 xff
    Nothing  -> stripPort . T.pack . show $ remoteHost req
  where
    stripPort t
      | Just inner <- T.stripPrefix "[" t = T.takeWhile (/= ']') inner
      | otherwise                          = T.takeWhile (/= ':') t


checkRL :: RateLimiter -> Request -> Handler ()
checkRL rl req = do
  ok <- liftIO $ RateLimit.check rl (clientIP req)
  if ok then pure () else throwError err429 { errBody = "Too many requests" }


rateLimitLoginHandler :: Env -> AuthHandler Request ConfirmRateLimitLogin
rateLimitLoginHandler env = mkAuthHandler \req ->
  checkRL env.rateLimiters.login req >> pure ConfirmRateLimitLogin

rateLimitUploadHandler :: Env -> AuthHandler Request ConfirmRateLimitUpload
rateLimitUploadHandler env = mkAuthHandler \req ->
  checkRL env.rateLimiters.upload req >> pure ConfirmRateLimitUpload

rateLimitDownloadHandler :: Env -> AuthHandler Request ConfirmRateLimitDownload
rateLimitDownloadHandler env = mkAuthHandler \req ->
  checkRL env.rateLimiters.download req >> pure ConfirmRateLimitDownload

rateLimitSearchHandler :: Env -> AuthHandler Request ConfirmRateLimitSearch
rateLimitSearchHandler env = mkAuthHandler \req ->
  checkRL env.rateLimiters.search req >> pure ConfirmRateLimitSearch

rateLimitSharedLinkHandler :: Env -> AuthHandler Request ConfirmRateLimitSharedLink
rateLimitSharedLinkHandler env = mkAuthHandler \req ->
  checkRL env.rateLimiters.sharedLink req >> pure ConfirmRateLimitSharedLink
