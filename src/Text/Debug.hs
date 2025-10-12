module Text.Debug where


import Text.Pretty.Simple (pShow)
import Data.Text.Lazy (Text)


class Debug a where
  debug :: a -> String
  debugP :: a -> Text

  debugP = pShow . debug
  {-# MINIMAL debug #-}
