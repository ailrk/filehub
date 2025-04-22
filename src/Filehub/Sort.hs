module Filehub.Sort where


sortFiles :: SortFileBy -> [File] -> [File]
sortFiles ByNameUp = sortOn (takeFileName . (.path))
sortFiles ByNameDown = reverse . sortFiles ByNameUp
sortFiles ByModifiedUp = sortOn (.mtime)
sortFiles ByModifiedDown = reverse . sortFiles ByModifiedUp
sortFiles BySizeUp = sortOn (.size)
sortFiles BySizeDown = reverse . sortFiles BySizeUp
