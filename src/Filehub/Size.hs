{-# LANGUAGE MultiWayIf #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- File size formatting.
module Filehub.Size (toReadableSize) where
import Text.Printf (printf)

toReadableSize :: Integer -> String
toReadableSize nbytes =
  if | nB  == 0  -> "0 B"
     | nTb >= 1  -> printf "%.1f TB" (nTb :: Double)
     | nGb >= 1  -> printf "%.1f GB" (nGb :: Double)
     | nMb >= 1  -> printf "%.1f MB" (nMb :: Double)
     | nKb >= 1  -> printf "%.1f KB" (nKb :: Double)
     | nB  >= 1  -> printf "%.0f B" (nB :: Double)
     | otherwise -> "unknown"
  where
    nB  = fromIntegral nbytes
    nKb = fromIntegral nbytes / (2 ^ 10)
    nMb = fromIntegral nbytes / (2 ^ 20)
    nGb = fromIntegral nbytes / (2 ^ 30)
    nTb = fromIntegral nbytes / (2 ^ 40)
