module Filehub.Domain.ClientPath
  ( toClientPath
  , fromClientPath
  )
  where


import Data.List ((\\))
import System.FilePath ((</>))
import Filehub.Domain.Types (ClientPath(..))


toClientPath :: FilePath -> FilePath -> ClientPath
toClientPath root path =
  let p = path \\ root
   in ClientPath $
     case p of
       '/':p' -> p'
       _ -> '/' : p


fromClientPath :: FilePath -> ClientPath -> FilePath
fromClientPath root (ClientPath cp) =
  root </>
    case cp of
      '/': cp' -> cp'
      _ -> cp
