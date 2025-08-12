{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ( ClientPath(..)
  , RawClientPath(..)
  , toClientPath
  , fromClientPath
  , toRawClientPath
  , fromRawClientPath
  )
  where


import System.FilePath ((</>), normalise)
import Network.URI.Encode qualified as URI.Encode
import Data.List (stripPrefix)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Data.Aeson (ToJSON(..))

-- | Filepath without the root part. The path is percent encoded safe to show in the frontend.
newtype ClientPath = ClientPath { unClientPath :: FilePath }
  deriving (Show, Eq, Semigroup, Monoid)


-- | ClientPath but not percent encoded
newtype RawClientPath = RawClientPath { unRawClientPath :: FilePath }
  deriving (Show, Eq, Semigroup, Monoid)


instance ToHttpApiData ClientPath where
  toUrlPiece (ClientPath p) = toUrlPiece p


instance FromHttpApiData ClientPath where
  parseUrlPiece p = ClientPath <$> parseUrlPiece p


instance ToJSON ClientPath where
  toJSON = toJSON . toUrlPiece


-- | Convert a file path into a ClientPath.
toClientPath :: FilePath -> FilePath -> ClientPath
toClientPath root path =
  let RawClientPath rcp = toRawClientPath root path
   in ClientPath (URI.Encode.encode rcp)


fromClientPath :: FilePath -> ClientPath -> FilePath
fromClientPath root (ClientPath cp) =
  let decoded = URI.Encode.decode cp
   in fromRawClientPath root (RawClientPath decoded)


-- | Remove the root part from the path, don't encode any characters.
--   root should starts with '/'. If not toRawClientPath will append one before stripping.
--   The path returned is guaranteed not start with '/'
toRawClientPath :: FilePath -> FilePath -> RawClientPath
toRawClientPath root path = do
  RawClientPath (ensureUnslash $ removePrefix (ensureSlash root) path)
  where


fromRawClientPath :: FilePath -> RawClientPath -> FilePath
fromRawClientPath root (RawClientPath cp) = root </> ensureUnslash cp


removePrefix :: FilePath -> FilePath -> FilePath
removePrefix prefix path =
  case stripPrefix (normalise prefix) (normalise path) of
    Just removed -> removed
    Nothing -> path


ensureSlash :: FilePath -> FilePath
ensureSlash path =
  case path of
    '/':_ -> path
    _ -> '/':path


ensureUnslash :: FilePath -> FilePath
ensureUnslash path =
  case path of
    '/':rest -> rest
    _ -> path
