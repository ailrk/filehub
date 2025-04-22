module Filehub.Sort (sortFiles) where

import Filehub.Types ( SortFileBy(..), File(size, path, mtime) )
import Data.List ( sortOn )
import System.FilePath ( takeFileName )


sortFiles :: SortFileBy -> [File] -> [File]
sortFiles ByNameUp = sortOn (takeFileName . (.path))
sortFiles ByNameDown = reverse . sortFiles ByNameUp
sortFiles ByModifiedUp = sortOn (.mtime)
sortFiles ByModifiedDown = reverse . sortFiles ByModifiedUp
sortFiles BySizeUp = sortOn (.size)
sortFiles BySizeDown = reverse . sortFiles BySizeUp
