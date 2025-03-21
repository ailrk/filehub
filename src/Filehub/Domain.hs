{-# LANGUAGE MultiWayIf #-}
module Filehub.Domain
  ( module Filehub.Domain.Theme
  , module Filehub.Domain.File
  , module Filehub.Domain.ClientPath
  , module Filehub.Domain.Mime
  , module Filehub.Domain.Viewer
  , module Filehub.Domain.Types
  , toReadableSize
  )
  where

import Filehub.Domain.File
import Filehub.Domain.Theme
import Filehub.Domain.ClientPath
import Filehub.Domain.Mime
import Filehub.Domain.Viewer
import Filehub.Domain.Types


import Text.Printf (printf)


------------------------------------
-- Readable size
------------------------------------


toReadableSize :: Integer -> String
toReadableSize nbytes =
  if | nB == 0 -> "0b"
     | nTb >= 1 -> printf "%.1fTB" (nTb :: Double)
     | nGb >= 1 -> printf "%.1fGB" (nGb :: Double)
     | nMb >= 1 -> printf "%.1fMB" (nMb :: Double)
     | nKb >= 1 -> printf "%.1fKB" (nKb :: Double)
     | nB >= 1 -> printf "%.0fB" (nB :: Double)
     | otherwise -> "unknown"

  where
    nB  = fromIntegral nbytes
    nKb = fromIntegral nbytes / (2 ^ 10)
    nMb = fromIntegral nbytes / (2 ^ 20)
    nGb = fromIntegral nbytes / (2 ^ 30)
    nTb = fromIntegral nbytes / (2 ^ 40)
