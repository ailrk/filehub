module System.FilePath.Extended where

import System.Environment (lookupEnv)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)


-- |  A simple util to expand environment variables embeded in a filepath.
expandVars :: FilePath -> IO FilePath
expandVars path
  | not ('$' `elem` path) = pure path
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
