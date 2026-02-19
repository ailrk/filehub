{-# LANGUAGE CPP #-}
module Filehub.Target where

import Effectful (IOE, (:>), Eff, MonadIO (..))
import Effectful.Log (Log, logInfo_)
import Target.Types (TargetId (..))
import Data.Text (Text)
import Amazonka qualified
import Data.ClientPath (AbsPath (..))
import Text.Debug (Debug (..))
import Effectful.FileSystem (FileSystem, makeAbsolute)
import Data.String.Interpolate (i)
import Data.UUID.V4 qualified as UUID

import Data.ByteString.Char8 qualified as ByteString
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Network.URI qualified as URI
import System.Environment qualified as Environment
import Data.Functor ((<&>))
import Amazonka.S3 qualified as Amazonka
import Data.Text qualified as Text
import GHC.Generics (Generic)

#ifdef DEBUG
import Effectful.FileSystem.IO (stdout)
import Amazonka.Env (Env'(..))
#endif


data Path
  = FileSysPath AbsPath
  | S3Path FilePath


data FileSysTarget = FileSysTarget
  { targetId   :: TargetId
  , targetName :: Maybe Text
  , root       :: AbsPath
  }


data FileSysConfig = FileSysConfig  { root :: String } deriving (Show, Eq, Generic, Debug)


data S3Config = S3Config { bucket :: String } deriving (Show, Eq, Generic, Debug)


data S3Target = S3Target
  { targetId :: TargetId
  , bucket   :: Text
  , env      :: Amazonka.Env
  }


initializeFileSys :: (IOE :> es, Log :> es, FileSystem :> es) => FileSysConfig -> Eff es FileSysTarget
initializeFileSys opt = do
  targetId <- liftIO $ TargetId <$> UUID.nextRandom
  root     <- AbsPath <$> makeAbsolute opt.root
  logInfo_ [i|[99s5fd] Initialized: #{targetId} - FS #{root}|]
  pure $ FileSysTarget targetId Nothing root



-- | The default `discover` method only discover `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`,
--   and `AWS_SESSION_TOKEN`. To set custom endpoint url, we also need to hand `AWS_ENDPOINT_URL`.
initializeS3 :: (IOE :> es, Log :> es) => S3Config -> Eff es S3Target
initializeS3 opt = do
  targetId   <- liftIO $ TargetId <$> UUID.nextRandom
  let bucket =  Text.pack opt.bucket
  service    <- liftIO makeS3Service
  env        <- liftIO do Amazonka.configureService service <$> Amazonka.newEnv Amazonka.discover
  logInfo_ [i|[ive9d4] Initialized: #{targetId} - S3 #{bucket}|]
#ifdef DEBUG
  logger <- Amazonka.newLogger Amazonka.Debug stdout
  pure $ S3Target bucket $ env { logger = logger }
#else
  pure $ S3Target targetId bucket env
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
