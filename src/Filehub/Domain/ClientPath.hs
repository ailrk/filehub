-- | Conceal the full absolute path by splitting a absolute path into
--   root part and the client part, the client part can be rendered safely in the frontend.
--
--   Note: S3 path is already absolute and fully qualified. Because root of S3 bucket is always "",
--   client path acts like a noop.
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
