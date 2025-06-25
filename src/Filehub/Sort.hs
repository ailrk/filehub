module Filehub.Sort (sortFiles) where

import Filehub.Types
  ( SortFileBy(..),
    File(..),
    FileContent (..))
import Data.List ( sortOn )
import System.FilePath ( takeFileName )


byFileNamewithDirFirst :: File -> String
byFileNamewithDirFirst file = do
  let pre = case file.content of
              Content -> '1'
              Dir _ -> '0'
  let name = takeFileName file.path :: String
  pre : name


sortFiles :: SortFileBy -> [File] -> [File]
sortFiles ByNameUp = sortOn byFileNamewithDirFirst
sortFiles ByNameDown = reverse . sortFiles ByNameUp
sortFiles ByModifiedUp = sortOn (.mtime)
sortFiles ByModifiedDown = reverse . sortFiles ByModifiedUp
sortFiles BySizeUp = sortOn (.size)
sortFiles BySizeDown = reverse . sortFiles BySizeUp
