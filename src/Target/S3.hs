{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
module Target.S3 where

import Target.Types (TargetId(..), TargetBackend, IsTarget(..))
import Data.Text (Text)
import Amazonka.Env qualified as Amazonka
import Effectful (IOE, (:>), Eff, MonadIO (..))
import Effectful.Log (Log, logInfo_)
import Data.String.Interpolate (i)
import Amazonka.Types qualified as Amazonka
import Data.UUID.V4 qualified as UUID
import Amazonka qualified
import Data.ByteString.Char8 qualified as ByteString
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Network.URI qualified as URI
import System.Environment qualified as Environment
import Data.Functor ((<&>))
import Amazonka.S3 qualified as Amazonka
import Data.Text qualified as Text
import Text.Debug (Debug(..))

#ifdef DEBUG
import Effectful.FileSystem.IO (stdout)
import Amazonka.Env (Env'(..))
#endif


data S3


data instance TargetBackend S3 = S3Backend
  { targetId :: TargetId
  , bucket   :: Text
  , env      :: Amazonka.Env
  }


instance Debug (TargetBackend S3) where
  debug S3Backend { targetId, bucket } =
    mconcat
      [ "[<S3Backend>, "
      , debug targetId, ", "
      , Text.unpack bucket
      , "]"
      ]


instance IsTarget S3 where
  getTargetIdFromBackend S3Backend { targetId } = targetId


data Config = Config
  { bucket :: String
  }
  deriving (Show, Eq)


instance Debug Config where debug = show


-- | The default `discover` method only discover `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`,
--   and `AWS_SESSION_TOKEN`. To set custom endpoint url, we also need to hand `AWS_ENDPOINT_URL`.
initialize :: (IOE :> es, Log :> es) => Config -> Eff es (TargetBackend S3)
initialize opt = do
  targetId   <- liftIO $ TargetId <$> UUID.nextRandom
  let bucket =  Text.pack opt.bucket
  service    <- liftIO makeS3Service
  env        <- liftIO do Amazonka.configureService service <$> Amazonka.newEnv Amazonka.discover
  logInfo_ [i|[ive9d4] Initialized: #{targetId} - S3 #{bucket}|]
#ifdef DEBUG
  logger <- Amazonka.newLogger Amazonka.Debug stdout
  pure $ S3Backend targetId bucket $ env { logger = logger }
#else
  pure $ S3Backend targetId bucket env
#endif
  where
    makeS3Service :: IO Amazonka.Service
    makeS3Service = do
      mUrl <- lookupAWSEndpointURL
      let setEndpointURL =
            case mUrl of
              Just url ->
                case url.uriAuthority of
                  Nothing -> id
                  Just auth -> do
                    let host = ByteString.pack auth.uriRegName
                    let port = fromMaybe 443 . readMaybe $ auth.uriPort
                    Amazonka.setEndpoint True host port
              Nothing -> id
      pure $ setEndpointURL Amazonka.defaultService

      where
        lookupAWSEndpointURL = do
          Environment.lookupEnv "AWS_ENDPOINT_URL" <&> \case
            Nothing -> Nothing
            Just "" -> Nothing
            Just v  -> URI.parseURI $ v
