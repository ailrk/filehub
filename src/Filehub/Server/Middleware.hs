{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Filehub.Server.Middleware
  ( exposeHeaders
  , dedupHeadersKeepLast
  , displayMiddleware
  , sessionMiddleware
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.CaseInsensitive qualified as CI
import Data.List (intersperse)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.UUID qualified as UUID
import Effectful ( MonadIO(liftIO), liftIO, liftIO )
import Effectful.Error.Dynamic (runErrorNoCallStack)
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env (..))
import Filehub.Env qualified as Env
import Filehub.Error ( withServerError, withServerError )
import Filehub.Error (FilehubError)
import Filehub.Monad ( toIO )
import Filehub.Server.Internal (parseHeader')
import Filehub.SessionPool qualified as SessionPool
import Filehub.Types (Session(..), SessionId(..))
import Filehub.UserAgent qualified as UserAgent
import Lens.Micro
import Lens.Micro.Platform ()
import Lens.Micro.Platform ()
import Lens.Micro.Platform ()
import Log (logTrace_)
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types (hUserAgent, status500)
import Network.Wai
import Network.Wai.Middleware.AddHeaders
import Prelude hiding (readFile)


exposeHeaders :: Middleware
exposeHeaders = addHeaders
  [ ("Access-Control-Expose-Headers", headers)
  ]
  where
    headers :: ByteString
    headers
      = ByteString.pack
      $ mconcat
      $ intersperse ","
      [ "X-Filehub-Selected-Count"
      ]


dedupHeadersKeepLast :: Middleware
dedupHeadersKeepLast app req respond =
  app req $ \res ->
    respond $ mapResponseHeaders keepLastHeaders res
  where
    -- Keep the last occurrence of each header
    keepLastHeaders :: [(HeaderName, ByteString)] -> [(HeaderName, ByteString)]
    keepLastHeaders = reverse . go Set.empty . reverse
      where
        go _ [] = []
        go seen ((k,v):xs)
          | CI.foldedCase k `Set.member` seen = go seen xs
          | otherwise = (k,v) : go (Set.insert (CI.foldedCase k) seen) xs


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
