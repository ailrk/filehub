{-# LANGUAGE ViewPatterns #-}
module Filehub.Theme
  ( Theme(..)
  , defaultTheme
  , parse
  ) where

-- An example theme file
--  :root {
--      --frontground: #DCD7BA;
--      --background1: #2d2d2d;
--      --background2: #262626;
--      --background3: #3b3b3b;
--      --primary: #E6C384;
--      --secondary: #E46876;
--      --tertiary: #FFA066;
--  }
--
-- All theme files should have the same format.

import Text.ParserCombinators.ReadP
import Data.Char (isAlphaNum, isSpace)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString


data Theme = Dark | Light

instance Show Theme where
  show = \case
    Dark -> "dark"
    Light -> "light"


instance Read Theme where
  readsPrec _ s = do
    let theme =
          case s of
          "dark" -> Dark
          "light" -> Light
          _ -> Dark
    pure (theme, "")


defaultTheme :: Theme
defaultTheme = Dark


-- Parse CSS variable definition
cssVar :: ReadP (String, String)
cssVar = do
  skipSpaces
  name <- munch1 (\c -> isAlphaNum c || c == '-')
  skipSpaces
  _ <- char ':'
  skipSpaces
  val <- munch1 (\c -> not (isSpace c) && c /= ';')
  _ <- char ';'
  skipSpaces
  return (name, val)


-- Parse multiple definitions in a :root block
cssVars :: ReadP [(String, String)]
cssVars = between (string ":root" >> skipSpaces >> char '{') (char '}') (many cssVar)


-- Extract background1 value from full text
parse :: String -> ByteString -> Maybe String
parse color txt =
  case readP_to_S cssVars (ByteString.unpack txt) of
    [] -> Nothing
    results -> lookup color (fst (last results))
