{-# LANGUAGE NumericUnderscores #-}
module Main where

import Criterion.Main (bench, bgroup, defaultMain, nfIO)
import Data.Text qualified as T
import Filehub.RateLimit qualified as RateLimit
import UnliftIO.Async (mapConcurrently)


main :: IO ()
main = do
  rlB1 <- RateLimit.new 1_000_000 60
  rlB2 <- RateLimit.new 1_000_000 60
  defaultMain
    [ bgroup "RateLimit.check"
        [ bench "single key"             $ nfIO (RateLimit.check rlB1 "bench-ip")
        , bench "10 distinct keys (par)" $ nfIO $
            mapConcurrently
              (\i -> RateLimit.check rlB2 (T.pack (show (i :: Int))))
              [1 .. 10]
        ]
    ]
