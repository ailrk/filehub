{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Target.S3 where

import Filehub.Prelude
import Target.Types (TargetId(..), Target, IsTarget(..), HasTargetId(..))
import Amazonka.Env qualified as Amazonka
import Amazonka.Types qualified as Amazonka
import Data.UUID.V4 qualified as UUID
import Amazonka qualified
import Text.Read (readMaybe)
import Network.URI qualified as URI
import System.Environment qualified as Environment
import Amazonka.S3 qualified as Amazonka
import Text.Debug (Debug(..))
import Log (logInfo_, MonadLog)

#ifdef DEBUG
import UnliftIO (stdout)
import Amazonka.Env (Env'(..))
#endif


data S3


instance Debug (Target S3) where
  debug S3Backend { targetId, bucket } =
    mconcat
      [ "[<S3Backend>, "
      , debug targetId, ", "
      , txt_unpack bucket
      , "]"
      ]


instance HasTargetId (Target S3) where
  getTargetId S3Backend { targetId } = targetId


instance IsTarget S3 where
  data instance Target S3 = S3Backend
    { targetId :: TargetId
    , bucket   :: Text
    , env      :: Amazonka.Env
    }

  data instance Config S3 = Config
    { bucket :: String
    }
    deriving (Show, Eq, Generic)


instance Debug (Config S3) where debug = show


-- | The default `discover` method only discover `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`,
--   and `AWS_SESSION_TOKEN`. To set custom endpoint url, we also need to hand `AWS_ENDPOINT_URL`.
initialize :: (MonadLog m, MonadIO m) => Config S3 -> m (Target S3)
initialize opt = do
  targetId   <- liftIO $ TargetId <$> UUID.nextRandom
  let bucket =  txt_pack opt.bucket
  service    <- liftIO makeS3Service
  env        <- liftIO do Amazonka.configureService service <$> Amazonka.newEnv Amazonka.discover
  logInfo_ [i|[ive9d4] Initialized: #{targetId} - S3 #{bucket}|]
#ifdef DEBUG
  logger <- Amazonka.newLogger Amazonka.Info stdout
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
                    let host = bs_packChars auth.uriRegName
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
