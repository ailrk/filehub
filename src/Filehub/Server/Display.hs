-- | set the display in cookie
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Filehub.Server.Display where

import Effectful ( MonadIO(liftIO), liftIO, liftIO )
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env (..))
import Filehub.Env qualified as Env
import Filehub.Error ( withServerError, withServerError )
import Filehub.Monad ( runFilehub )
import Lens.Micro
import Lens.Micro.Platform ()
import Network.Wai
import Prelude hiding (readFile)
import Log (runLogT)
import Filehub.Server.Internal (parseHeader')
import Lens.Micro.Platform ()
import Filehub.Types (Display(..))


displayMiddleware :: Env -> Middleware
displayMiddleware  env@Env{ logger, logLevel } app req respond = runLogT "sessionMiddleware" logger logLevel $ do
  let mCookie = lookup "Cookie" $ requestHeaders req
  let Just sessionId = mCookie >>= parseHeader' >>= Cookies.getSessionId
  result <- liftIO . runFilehub env $ do
    Env.getDisplay sessionId & withServerError
  let setCookieHeader =
        case result of
          Left _ -> ("Set-Cookie", Cookies.renderSetCookie $ Cookies.setDisplay NoDisplay)
          Right display -> ("Set-Cookie", Cookies.renderSetCookie $ Cookies.setDisplay display)
  liftIO $ app req $ \res ->
    let res' = mapResponseHeaders (setCookieHeader :) res
     in respond res'
