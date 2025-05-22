-- | set the display in cookie
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Filehub.Server.Display where

import Effectful ( MonadIO(liftIO), liftIO, liftIO )
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env (..))
import Filehub.Env qualified as Env
import Filehub.Error ( withServerError, withServerError )
import Filehub.Monad ( toIO )
import Filehub.UserAgent qualified as UserAgent
import Filehub.SessionPool qualified as SessionPool
import Lens.Micro
import Lens.Micro.Platform ()
import Network.Wai
import Prelude hiding (readFile)
import Filehub.Server.Internal (parseHeader')
import Lens.Micro.Platform ()
import Network.HTTP.Types (hUserAgent, status500)
import Control.Monad (when)


displayMiddleware :: Env -> Middleware
displayMiddleware  env app req respond = toIO onErr env do
  let mCookie = lookup "Cookie" $ requestHeaders req
  let Just sessionId = mCookie >>= parseHeader' >>= Cookies.getSessionId
  session <- SessionPool.getSession sessionId & withServerError

  -- set device type
  do
    let mUserAgent = lookup hUserAgent $ requestHeaders req
    let deviceType =
          case mUserAgent of
            Just userAgent -> UserAgent.detectDeviceType userAgent
            Nothing -> UserAgent.Unknown
    when (session ^. #deviceType /= deviceType) do
      SessionPool.updateSession sessionId $ #deviceType .~ deviceType


  -- set display cookie
  -- Note only the server set the cookie.
  setCookieHeader <- do
    currentDisplay <- Env.getDisplay sessionId & withServerError
    let header = ("Set-Cookie", Cookies.renderSetCookie $ Cookies.setDisplay currentDisplay)
    pure (header :)

  liftIO $ app req $ \res ->
    let res' = mapResponseHeaders setCookieHeader res
     in respond res'

  where
    onErr _ = respond $ responseLBS (status500) [] "invalid display information"
