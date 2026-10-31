-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- The custom logger for filehub.
module Filehub.Log (withColoredStdoutLogger) where

import System.IO (stdout, hFlush)
import Data.Time (defaultTimeLocale, UTCTime)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.ByteString qualified as B
import Log.Internal.Logger (withLogger, Logger)
import Data.Aeson (Value)
import Data.Time.Format (formatTime)
import Data.Aeson.Types (emptyObject)
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', Indent (..))
import UnliftIO (MonadUnliftIO (..))
import Log.Data (LogMessage (..), LogLevel (..), showLogLevel)
import Log (mkLogger)


withColoredStdoutLogger :: MonadUnliftIO m => (Logger -> m r) -> m r
withColoredStdoutLogger act = withRunInIO \unlift -> do
  logger <- mkLogger "stdout" \msg -> do
    T.putStrLn (showColoredLogMessage Nothing msg)
    hFlush stdout
  withLogger logger (unlift . act)


-- | Render a 'LogMessage' to 'Text'.
showColoredLogMessage :: Maybe UTCTime -> LogMessage -> Text
showColoredLogMessage mInsertionTime LogMessage{..}
  = T.concat $ [
    T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lmTime)
  , case mInsertionTime of
      Nothing -> " "
      Just it -> T.pack (formatTime defaultTimeLocale " (%H:%M:%S) " it)
  , colorize lmLevel . T.toUpper $ showLogLevel lmLevel
  , " "
  , colorize lmLevel . T.intercalate "/" $ lmComponent : lmDomain
  , ": "
  , lmMessage
  ] ++ if lmData == emptyObject
    then []
    else [" ", textifyData lmData]
  where
    textifyData :: Value -> Text
    textifyData = T.decodeUtf8 . B.toStrict . encodePretty' defConfig {
      confIndent = Spaces 2
    }


colorize :: LogLevel -> Text -> Text
colorize level txt = colorCode level <> txt <> reset
  where
    reset = "\x1b[0m"


colorCode :: LogLevel -> Text
colorCode level = case level of
  LogTrace     -> "\x1b[93m"
  LogInfo      -> "\x1b[96m"
  LogAttention -> "\x1b[91m"
