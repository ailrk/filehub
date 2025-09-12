module Filehub.Log (withColoredStdoutLogger) where

import Effectful
import Effectful.Log (Logger, LogLevel (..), mkLogger, UTCTime, LogMessage (..), showLogLevel)
import System.IO (stdout, hFlush)
import Data.Time (defaultTimeLocale)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString qualified as ByteString
import Log.Internal.Logger (withLogger)
import Data.Aeson (Value)
import Data.Time.Format (formatTime)
import Data.Aeson.Types (emptyObject)
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', Indent (..))


withColoredStdoutLogger :: MonadUnliftIO m => (Logger -> m r) -> m r
withColoredStdoutLogger act = withRunInIO \unlift -> do
  logger <- mkLogger "stdout" \msg -> do
    Text.putStrLn (showColoredLogMessage Nothing msg)
    hFlush stdout
  withLogger logger (unlift . act)


-- | Render a 'LogMessage' to 'Text'.
showColoredLogMessage :: Maybe UTCTime -> LogMessage -> Text
showColoredLogMessage mInsertionTime LogMessage{..}
  = Text.concat $ [
    Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lmTime)
  , case mInsertionTime of
      Nothing -> " "
      Just it -> Text.pack (formatTime defaultTimeLocale " (%H:%M:%S) " it)
  , colorize lmLevel . Text.toUpper $ showLogLevel lmLevel
  , " "
  , colorize lmLevel . Text.intercalate "/" $ lmComponent : lmDomain
  , ": "
  , lmMessage
  ] ++ if lmData == emptyObject
    then []
    else [" ", textifyData lmData]
  where
    textifyData :: Value -> Text
    textifyData = Text.decodeUtf8 . ByteString.toStrict . encodePretty' defConfig {
      confIndent = Spaces 2
    }


colorize :: LogLevel -> Text -> Text
colorize level txt = colorCode level <> txt <> reset
  where
    reset = "\x1b[0m"


colorCode :: LogLevel -> Text
colorCode level = case level of
  LogTrace     -> "\x1b[90m"  -- bright black (gray)
  LogInfo      -> "\x1b[94m"  -- bright blue
  LogAttention -> "\x1b[91m"  -- bright red
