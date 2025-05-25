{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Middleware.Session
  ( sessionMiddleware
  )
  where

import Data.String.Interpolate (i)
import Data.UUID qualified as UUID
import Effectful ( MonadIO(liftIO))
import Filehub.Env (Env(..))
import Filehub.Error (FilehubError)
import Filehub.SessionPool qualified as SessionPool
import Filehub.Monad (toIO)
import Filehub.Server.Internal (parseHeader')
import Filehub.Cookie qualified as Cookies
import Filehub.Types
    ( Session(..),
      SessionId(..))
import Network.Wai
import Prelude hiding (readFile)
import Log (logTrace_)
import Lens.Micro.Platform ()
import Network.HTTP.Types (status500)
import Effectful.Error.Dynamic (runErrorNoCallStack)


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
