{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module defines several filehub specifci WAI middlewares.
-- These middlewares sit in front of the Servant handlers, allowing
-- them to modify requests and responses.
module Network.Wai.Middleware.Filehub where

import Control.Monad (when)
import Data.String.Interpolate (i)
import Data.UUID qualified as UUID
import Effectful ( MonadIO(liftIO), liftIO, liftIO )
import Effectful.Error.Dynamic (runErrorNoCallStack)
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env (..))
import Filehub.Error (FilehubError)
import Filehub.Monad ( toIO )
import Filehub.Server.Internal (parseHeader')
import Filehub.Session qualified as Session
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Types (Session(..), SessionId(..))
import Filehub.UserAgent qualified as UserAgent
import Lens.Micro
import Lens.Micro.Platform ()
import Log (logTrace_)
import Network.HTTP.Types (hUserAgent, status500)
import Network.HTTP.Types.Header (hSetCookie)
import Network.Wai
import Prelude hiding (readFile)


displayMiddleware :: Env -> Middleware
displayMiddleware  env app req respond = toIO onErr env do
  let mCookie        = lookup "Cookie" (requestHeaders req)
  let Just sessionId = mCookie >>= parseHeader' >>= Cookies.getSessionId
  session <- Session.Pool.get sessionId

  -- set device type
  do
    let mUserAgent = lookup hUserAgent (requestHeaders req)
    let deviceType =
          case mUserAgent of
            Just userAgent -> UserAgent.detectDeviceType userAgent
            Nothing        -> UserAgent.Unknown
    when (session ^. #deviceType /= deviceType) do
      Session.Pool.update sessionId (#deviceType .~ deviceType)

  -- set display cookie
  -- Note only the server set the cookie.
  setCookieHeader <- do
    currentDisplay <- Session.getDisplay sessionId
    let header     =  ("Set-Cookie", Cookies.renderSetCookie (Cookies.setDisplay currentDisplay))
    pure (header :)

  liftIO $ app req \res ->
    let res' = mapResponseHeaders setCookieHeader res
     in respond res'

  where
    onErr _ = respond $ responseLBS (status500) [] "invalid display information"


-- | If session is not present, create a new session
sessionMiddleware :: Env -> Middleware
sessionMiddleware env app req respond = toIO onErr env do
  let mCookie    = lookup "Cookie" (requestHeaders req)
  let mSessionId = mCookie >>= parseHeader' >>= Cookies.getSessionId
  case mSessionId of
    Just sessionId -> do
      eSession <- runErrorNoCallStack @FilehubError $ Session.Pool.get sessionId
      case eSession of
        Left _ -> do
          respondWithNewSession
        Right _ -> do
          liftIO $ app req respond
    Nothing -> do
      logTrace_ [i|No session found.|]
      respondWithNewSession
  where
    respondWithNewSession = do
      session <- Session.Pool.newSession
      let sessionId@(SessionId sid) = session.sessionId
      let setCookieHeader           = ("Set-Cookie", Cookies.renderSetCookie $ Cookies.setSessionId session)
      let injectedCookieHeader      = ("Cookie", "sessionId=" <> UUID.toASCIIBytes sid)
      let req'                      = req { requestHeaders = injectedCookieHeader : requestHeaders req }
      logTrace_ [i|New session: #{sessionId}|]
      liftIO $ app req' \res ->
        let res' = mapResponseHeaders (setCookieHeader :) res
         in respond res'
    onErr _ = respond $ responseLBS status500 [] "server error" -- impossible



-- | We want to strict all cookies on responds to static files, otherwise CDN will not cache these
--   content.
stripCookiesForStatic :: Middleware
stripCookiesForStatic app req respond
  | not (null path) && head path == "static" =
    app req \res -> do
      respond $ mapResponseHeaders (filter (\(h,_) -> h /= hSetCookie)) res
  | otherwise = app req respond
  where path = pathInfo req
