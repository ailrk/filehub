module Filehub.Env.Target.S3 where

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
import Text.URI (URI(..))
import Amazonka qualified
import Data.Maybe (fromMaybe)


-- | The default `discover` method only discover `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`,
--   and `AWS_SESSION_TOKEN`. To set custom endpoint url, we also need to hand `AWS_ENDPOINT_URL`.
initTarget :: MonadUnliftIO m => S3TargetOption -> m S3Target
initTarget to = liftIO $ do
  targetId <- TargetId <$> UUID.nextRandom
  let bucket = to.bucket
  env <- Amazonka.newEnv Amazonka.discover >>= setAwsEndpointURL
  pure $ S3Target_ targetId Nothing env bucket

  where
    setAwsEndpointURL env = do
      lookupAWSEndpointURL <&> \case
        Just url ->
          flip Amazonka.overrideService env
            $ \s ->
              fromMaybe s do
                host <- either (const Nothing) (Just . Text.encodeUtf8 . URI.unRText . (.authHost)) url.uriAuthority
                port <- either (const (Just 8000)) (\a -> fromIntegral <$> a.authPort) url.uriAuthority
                pure $ Amazonka.setEndpoint True host port s
        Nothing -> env

      where
        lookupAWSEndpointURL = do
          Environment.lookupEnv "AWS_ENDPOINT_URL" <&> \case
            Nothing -> Nothing
            Just "" -> Nothing
            Just v -> URI.mkURI . Text.pack $ v
