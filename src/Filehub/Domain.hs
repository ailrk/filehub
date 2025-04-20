{-# LANGUAGE MultiWayIf #-}
module Filehub.Domain
  ( module Filehub.Domain.Theme
  , module Filehub.Domain.ClientPath
  , module Filehub.Domain.Mime
  , module Filehub.Domain.Types
  , toReadableSize
  , sortFiles
  )
  where

import Filehub.Domain.Theme
import Filehub.Domain.ClientPath
import Filehub.Domain.Mime
import Filehub.Domain.Types
import Text.Printf (printf)
import Data.List (sortOn)
import System.FilePath (takeFileName)


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



sortFiles :: SortFileBy -> [File] -> [File]
sortFiles ByNameUp = sortOn (takeFileName . (.path))
sortFiles ByNameDown = reverse . sortFiles ByNameUp
sortFiles ByModifiedUp = sortOn (.mtime)
sortFiles ByModifiedDown = reverse . sortFiles ByModifiedUp
sortFiles BySizeUp = sortOn (.size)
sortFiles BySizeDown = reverse . sortFiles BySizeUp
