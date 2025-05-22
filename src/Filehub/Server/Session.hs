{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Session
  ( sessionHandler
  , sessionMiddleware
  )
  where

import Data.String.Interpolate (i)
import Data.UUID qualified as UUID
import Data.Bifunctor (Bifunctor(..))
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Effectful ( MonadIO(liftIO), liftIO, liftIO, runEff )
import Effectful.Reader.Dynamic (runReader)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant (FromHttpApiData (..), err401, throwError, Handler (..), errBody)
import Filehub.Types (SessionId)
import Filehub.Cookie qualified as Cookie
import Filehub.Env (Env(..))
import Filehub.Error (withServerError, FilehubError)
import Filehub.SessionPool qualified as SessionPool
import Filehub.Monad (toServantHandler, toIO)
import Filehub.Server.Internal (parseHeader')
import Filehub.Cookie qualified as Cookies
import Filehub.Types
    ( Session(..),
      SessionId(..))
import Network.Wai
import Prelude hiding (readFile)
import Log (logTrace_)
import Lens.Micro.Platform ()
import Lens.Micro
import Network.HTTP.Types (status500)
import Effectful.Error.Dynamic (runErrorNoCallStack)


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


-- | If session is not present, create a new session
sessionMiddleware :: Env -> Middleware
sessionMiddleware env app req respond = toIO onErr env do
  let mCookie = lookup "Cookie" $ requestHeaders req
  let mSessionId = mCookie >>= parseHeader' >>= Cookies.getSessionId
  case mSessionId of
    Just sessionId -> do
      eSession <- runErrorNoCallStack @FilehubError $ SessionPool.getSession sessionId
      case eSession of
        Left _ -> do
          logTrace_ [i|Invalid session: #{sessionId}|]
          respondWithNewSession
        Right _ -> do
          logTrace_ [i|Existed session, #{sessionId} |]
          liftIO $ app req respond
    Nothing -> do
      logTrace_ [i|No session found.|]
      respondWithNewSession
  where
    respondWithNewSession = do
      session <- SessionPool.newSession
      let sessionId@(SessionId sid) = session.sessionId
      let setCookieHeader = ("Set-Cookie", Cookies.renderSetCookie $ Cookies.setSessionId session)
      let injectedCookieHeader = ("Cookie", "sessionId=" <> UUID.toASCIIBytes sid)
      let req' = req { requestHeaders = injectedCookieHeader : requestHeaders req }
      logTrace_ [i|New session: #{sessionId}|]
      liftIO $ app req' $ \res ->
        let res' = mapResponseHeaders (setCookieHeader :) res
         in respond res'
    onErr _ = respond $ responseLBS status500 [] "server error" -- impossible
