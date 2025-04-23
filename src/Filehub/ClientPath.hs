-- | Conceal the full absolute path by splitting a absolute path into
--   root part and the client part, the client part can be rendered safely in the frontend.
--   ClientPath is percent encoded, the frontend code can safely display it in the UI.
--   When the server receives a ClientPath, it will restore it to normal path by first decode
--   the percent encoding, then append the root prefix.
--
--   ClientPath is unique within a directory.
--
--   Note: S3 path is already absolute and fully qualified. Because root of S3 bucket is always "",
--   client path acts like a noop.
module Filehub.ClientPath
  ( toClientPath
  , fromClientPath
  , toRawClientPath
  , fromRawClientPath
  )
  where


import Data.List ((\\))
import System.FilePath ((</>))
import Filehub.Types (ClientPath(..), RawClientPath(..))
import Network.URI.Encode qualified as URI.Encode


-- | Convert a file path into a ClientPath.
toClientPath :: FilePath -> FilePath -> ClientPath
toClientPath root path =
  let RawClientPath rcp = toRawClientPath root path
   in ClientPath (URI.Encode.encode rcp)


fromClientPath :: FilePath -> ClientPath -> FilePath
fromClientPath root (ClientPath cp) =
  let decoded = URI.Encode.decode cp
   in fromRawClientPath root (RawClientPath decoded)


toRawClientPath :: FilePath -> FilePath -> RawClientPath
toRawClientPath root path =
  let p = path \\ root
   in RawClientPath
     case p of
       '/':p' -> p'
       _ -> p


fromRawClientPath :: FilePath -> RawClientPath -> FilePath
fromRawClientPath root (RawClientPath cp) =
   root </>
     case cp of
       '/': rest -> rest
       _ -> cp
