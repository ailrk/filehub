module System.FilePath.Extended (expandVars, timeIt) where

import System.Environment (lookupEnv)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad.IO.Class (liftIO)
import UnliftIO (MonadIO)



-- |  A simple util to expand environment variables embeded in a filepath.
expandVars :: FilePath -> IO FilePath
expandVars path
  | '$' `notElem` path = pure path
expandVars ('$':'{':rest) = do
  let (var, rest') = span (/= '}') rest
  case rest' of
    ('}':rest'') -> do
      mVar <- lookupEnv var
      case mVar of
        Just x -> (x ++) <$> expandVars rest''
        Nothing -> pure ""
    _ -> do
      expanded <- expandVars rest
      pure $ "${" ++ expanded
expandVars ('$':rest) = do
  let (var, rest') = span isVarChar rest
  val <- fromMaybe "" <$> lookupEnv var
  (val ++) <$> expandVars rest'
expandVars (c:cs) = (c :) <$> expandVars cs
expandVars [] = pure []


isVarChar :: Char -> Bool
isVarChar x = isAlphaNum x || x == '_'


timeIt :: MonadIO m => String -> m a -> m a
timeIt label action = do
    start <- liftIO getCurrentTime
    val   <- action
    end   <- liftIO getCurrentTime
    liftIO $ putStrLn $ label ++ " took: " ++ show (diffUTCTime end start)
    pure val
