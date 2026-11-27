{-# LANGUAGE FieldSelectors #-}
module Text.Fuzzy (Fuzzy(..), filter) where

import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude hiding (filter)


data Fuzzy a = Fuzzy
  { original :: a
  , score    :: Int
  }


-- | Filter and score a list by how well each element matches the pattern.
-- Scoring rewards consecutive character matches.
filter :: Text -> [a] -> (a -> Text) -> Bool -> [Fuzzy a]
filter pat xs extract caseSensitive =
  [ Fuzzy x s | x <- xs, Just s <- [match (norm pat) (norm (extract x))] ]
  where
    norm = if caseSensitive then id else T.toLower


match :: Text -> Text -> Maybe Int
match pat str = go (T.unpack pat) (T.unpack str) 0 0
  where
    go []     _      score _   = Just score
    go _      []     _     _   = Nothing
    go (p:ps) (c:cs) score run
      | p == c    = go ps cs (score + 1 + run) (run + 1)
      | otherwise = go (p:ps) cs score 0
