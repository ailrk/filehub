module Filehub.Env.Target.S3 ( initTarget) where

import Filehub.Options ( S3TargetOption(..) )
import Filehub.Types
    ( S3Target(..),
      TargetId(..) )
import Data.UUID.V4 qualified as UUID
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Generics.Labels ()
import UnliftIO (MonadUnliftIO, MonadIO (..))
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import System.Environment qualified as Environment
import Text.URI qualified as URI
import Text.URI.Lens qualified as URI.Lens
import Amazonka qualified
import Amazonka.S3 qualified
import Data.Maybe (fromMaybe)


-- | The default `discover` method only discover `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`,
--   and `AWS_SESSION_TOKEN`. To set custom endpoint url, we also need to hand `AWS_ENDPOINT_URL`.
initTarget :: MonadUnliftIO m => S3TargetOption -> m S3Target
initTarget to = liftIO $ do
  targetId <- TargetId <$> UUID.nextRandom
  let bucket = Text.pack to.bucket
  service <- makeS3Service
  env <- Amazonka.configureService service
        <$> Amazonka.newEnv Amazonka.discover
  pure $ S3Target_ targetId bucket env
  where
    makeS3Service :: IO Amazonka.Service
    makeS3Service = do
      mUrl <- lookupAWSEndpointURL
      let setEndpointURL =
            case mUrl of
              Just url ->
                case url ^. URI.Lens.uriAuthority of
                  Left _ -> id
                  Right auth -> do
                    let host = Text.encodeUtf8 . URI.unRText $ auth ^. URI.Lens.authHost
                    let port = fromIntegral . fromMaybe 443 $ auth ^. URI.Lens.authPort
                    Amazonka.setEndpoint True host port
              Nothing -> id
      pure $ setEndpointURL Amazonka.S3.defaultService

      where
        lookupAWSEndpointURL = do
          Environment.lookupEnv "AWS_ENDPOINT_URL" <&> \case
            Nothing -> Nothing
            Just "" -> Nothing
            Just v -> URI.mkURI . Text.pack $ v
