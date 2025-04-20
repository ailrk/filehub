module Filehub.Server (dynamicRaw) where

import Network.Wai
import Network.Wai.Application.Static
import Servant
    ( Raw,
      Tagged(..),
      ServerT,
      ServerError(..))
import Effectful
    ( MonadIO(liftIO), liftIO )
import Filehub.Env (Env, TargetView (..))
import Filehub.Env qualified as Env
import Filehub.Monad (runFilehub)
import Filehub.Types (Target(..))
import Filehub.Domain.Types (File(..))
import Filehub.Cookie (getSessionId)
import Filehub.Error (withServerError, FilehubError (..), toServerError)
import Filehub.Storage qualified as Storage
import Data.ByteString.Char8 qualified as ByteString.Char8
import Web.Cookie (parseCookies)
import Lens.Micro.Platform ()
import Prelude hiding (readFile)
import Network.HTTP.Types.Status (mkStatus)


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
      let path = case ByteString.Char8.unpack req.rawPathInfo of
                   '/':rest -> rest
                   other -> other
      (file, bytes) <- Storage.runStorage sessionId $ do
        file <- Storage.getFile path
        bytes <- Storage.readFileContent file
        pure (file, bytes)
      liftIO $ do
        respond $
          responseLBS
            (mkStatus 200 "")
            [ ("Content-Type", file.mimetype)
            ]
            bytes
