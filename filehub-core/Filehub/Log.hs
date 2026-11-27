-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- The custom logger for filehub.
module Filehub.Log (withColoredStdoutLogger) where

import Filehub.Prelude
import System.IO (stdout, hFlush)
import Log.Internal.Logger (withLogger, Logger)
import Data.Aeson (Value)
import Data.Aeson.Types (emptyObject)
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', Indent (..))
import Log.Data (LogMessage (..), LogLevel (..), showLogLevel)
import Log (mkLogger)


withColoredStdoutLogger :: MonadUnliftIO m => (Logger -> m r) -> m r
withColoredStdoutLogger act = withRunInIO \unlift -> do
  logger <- mkLogger "stdout" \msg -> do
    txt_putStrLn (showColoredLogMessage Nothing msg)
    hFlush stdout
  withLogger logger (unlift . act)


-- | Render a 'LogMessage' to 'Text'.
showColoredLogMessage :: Maybe UTCTime -> LogMessage -> Text
showColoredLogMessage mInsertionTime msg
  = txt_concat $ [
    txt_pack (time_format "%Y-%m-%d %H:%M:%S" msg.lmTime)
  , case mInsertionTime of
      Nothing -> " "
      Just it -> txt_pack (time_format " (%H:%M:%S) " it)
  , colorize msg.lmLevel . txt_toUpper $ showLogLevel msg.lmLevel
  , " "
  , colorize msg.lmLevel . txt_intercalate "/" $ msg.lmComponent : msg.lmDomain
  , ": "
  , msg.lmMessage
  ] ++ if msg.lmData == emptyObject
    then []
    else [" ", textifyData msg.lmData]
  where
    textifyData :: Value -> Text
    textifyData = txt_decode . lBS_toStrict . encodePretty' defConfig {
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
