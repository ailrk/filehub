{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server
  ( server
  , dynamicRaw
  , sessionMiddleware
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.String.Interpolate (i)
import Data.UUID qualified as UUID
import Effectful ( MonadIO(liftIO), liftIO, liftIO, runEff )
import Effectful.Reader.Dynamic (runReader)
import Filehub.Cookie ( getSessionId, parseCookies )
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env (..), TargetView (..))
import Filehub.Env qualified as Env
import Filehub.SessionPool qualified as SessionPool
import Filehub.Error ( withServerError, FilehubError(..), toServerError, FilehubError(..), withServerError )
import Filehub.Monad ( runFilehub, Filehub )
import Filehub.Routes (Api (..))
import Filehub.Storage qualified as Storage
import Filehub.Types
    ( Target(..),
      Session(..),
      SessionId(..),
      File(..),
      Session(..),
      SessionId(..), Display (..), Resolution (..))
import Filehub.Server.Desktop qualified as Server.Desktop
import Filehub.Server.Mobile qualified as Server.Mobile
import Filehub.Template qualified as Template
import Lens.Micro
import Lens.Micro.Platform ()
import Network.HTTP.Types.Status (mkStatus)
import Network.Wai
import Network.Wai.Application.Static
import Prelude hiding (readFile)
import Servant ( Raw, Tagged(..), ServerT, ServerError(..), ServerError, FromHttpApiData (..) )
import Servant.Server.Generic (AsServerT)
import Log (runLogT, logTrace_)
import Network.URI.Encode qualified as URI
import Effectful.Log (logAttention_)


-- | Handle static file access
dynamicRaw :: Env -> Servant.ServerT Servant.Raw m
dynamicRaw env = Servant.Tagged $ \req respond -> do
  let mSessionId = do
        bytes <- lookup "Cookie" (requestHeaders req)
        getSessionId . parseCookies $ bytes
  case mSessionId of
    Just sessionId -> do
      mR <- runFilehub env $ withServerError $ do
        root <- Env.getRoot sessionId
        TargetView target _ _ <- Env.currentTarget sessionId
        case target of
          FileTarget _ -> throughFS root req respond
          S3Target _ -> throughS3 sessionId req respond
      case mR of
        Left err -> respond $ serverErrorToResponse err
        Right r -> pure r
    Nothing -> do
      let err = toServerError InvalidSession
      respond $ serverErrorToResponse err
  where
    serverErrorToResponse :: ServerError -> Response
    serverErrorToResponse err =
      responseLBS
        (mkStatus err.errHTTPCode "")
        err.errHeaders
        err.errBody

    -- File from file systems are simply served as a static app
    throughFS root req respond = liftIO $ do
      let app = staticApp (defaultWebAppSettings root)
      app req respond

    -- File from S3 are forwared from S3 to the client.
    throughS3 sessionId req respond = do
      let path = URI.decode
                $ case ByteString.Char8.unpack req.rawPathInfo of
                    '/':rest -> rest
                    other -> other
      (file, bytes) <- Storage.runStorage sessionId $ do
        file <- Storage.get path
        bytes <- Storage.read file
        pure (file, bytes)
      liftIO $ do
        respond $
          responseLBS
            (mkStatus 200 "")
            [ ("Content-Type", file.mimetype)
            ]
            bytes


parseHeader' :: FromHttpApiData a => ByteString -> Maybe a
parseHeader' x = either (const Nothing) Just $ parseHeader x


-- | If session is not present, create a new session
sessionMiddleware :: Env -> Middleware
sessionMiddleware env@Env{ logger, logLevel } app req respond = runLogT "sessionMiddleware" logger logLevel $ do
  let mCookie = lookup "Cookie" $ requestHeaders req
  let mSessionId = mCookie >>= parseHeader' >>= Cookies.getSessionId
  let mResolution = mCookie >>= parseHeader' >>= Cookies.getResolution
  case mSessionId of
    Just sessionId -> do
      eSession <- liftIO . runFilehub env $ SessionPool.getSession sessionId & withServerError
      case eSession of
        Left _ -> do
          logTrace_ [i|Invalid session: #{sessionId}|]
          respondWithNewSession
        Right session -> do
          logTrace_ [i|Existed session, #{sessionId} #{session ^. #resolution} #{mResolution}|]
          liftIO $ app req respond
    Nothing -> do
      logTrace_ [i|No session found.|]
      respondWithNewSession
  where
    respondWithNewSession = do
      session <- liftIO . runEff . runReader env $ SessionPool.newSession
      let sessionId@(SessionId sid) = session.sessionId
      let setCookieHeader = ("Set-Cookie", Cookies.renderSetCookie $ Cookies.setSessionId session)
      let injectedCookieHeader = ("Cookie", "sessionId=" <> UUID.toASCIIBytes sid)
      let req' = req { requestHeaders = injectedCookieHeader : requestHeaders req }
      logTrace_ [i|New session: #{sessionId}|]
      liftIO $ app req' $ \res ->
        let res' = mapResponseHeaders (setCookieHeader :) res
         in respond res'


-- | Server definition
server :: Api (AsServerT Filehub)
server = Api
  { init = \sessionId mRes -> do
      case mRes of
        Just res -> do
          Env.updateSession sessionId $
            \s -> s & #resolution .~ Just res
          server.index sessionId
        Nothing -> do
          logAttention_ "No resolution info from /init. Set to 360x800 as default"
          Env.updateSession sessionId $
            \s -> s & #resolution .~ Just (Resolution 360 800)
          server.index sessionId


  , index = \sessionId -> do
      Env.getDisplay sessionId & withServerError >>= \case
        NoDisplay -> do -- boostrap
          pure $ Template.bootstrap
        Desktop -> Server.Desktop.server.index sessionId
        Mobile -> Server.Mobile.server.index sessionId


  , cd = Server.Desktop.server.cd


  , newFile = Server.Desktop.server.newFile


  , updateFile = Server.Desktop.server.updateFile


  , deleteFile = Server.Desktop.server.deleteFile


  , newFolder = Server.Desktop.server.newFolder


  , newFileModal = Server.Desktop.server.newFileModal


  , newFolderModal = Server.Desktop.server.newFolderModal


  , fileDetailModal = Server.Desktop.server.fileDetailModal


  , editorModal = Server.Desktop.server.editorModal


  , search = Server.Desktop.server.search


  , sortTable = Server.Desktop.server.sortTable


  , selectRows = Server.Desktop.server.selectRows


  , upload = Server.Desktop.server.upload


  , download = Server.Desktop.server.download


  , copy = Server.Desktop.server.copy


  , paste = Server.Desktop.server.paste


  , cancel = Server.Desktop.server.cancel


  , contextMenu = Server.Desktop.server.contextMenu


  , initViewer = Server.Desktop.server.initViewer


  , changeTarget = Server.Desktop.server.changeTarget


  , themeCss = Server.Desktop.server.themeCss


  , healthz = pure "ok"
  }
