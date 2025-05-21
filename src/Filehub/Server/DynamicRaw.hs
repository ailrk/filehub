module Filehub.Server.DynamicRaw where

import Data.ByteString.Char8 qualified as ByteString.Char8
import Effectful ( MonadIO(liftIO), liftIO, liftIO )
import Filehub.Cookie ( getSessionId, parseCookies )
import Filehub.Env (Env (..), TargetView (..))
import Filehub.Env qualified as Env
import Filehub.Error ( withServerError, FilehubError(..), toServerError, FilehubError(..), withServerError )
import Filehub.Monad ( runFilehub )
import Filehub.Storage qualified as Storage
import Filehub.Types
    ( Target(..),
      File(..))
import Network.HTTP.Types.Status (mkStatus)
import Network.Wai
import Network.Wai.Application.Static
import Prelude hiding (readFile)
import Servant ( Raw, Tagged(..), ServerT, ServerError(..), ServerError)
import Network.URI.Encode qualified as URI
import Lens.Micro.Platform ()


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
