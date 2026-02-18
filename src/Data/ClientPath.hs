-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Conceal the full absolute path by splitting a absolute path into
-- prefix part and the client part, the client part can be rendered safely in the frontend.
-- ClientPath is percent encoded, the frontend code can safely display it in the UI.
-- When the server receives a ClientPath, it will restore it to normal path by first decode
-- the percent encoding, then append the prefix prefix.
--
-- ClientPath is unique within a directory.
--
-- Note: S3 path is already absolute and fully qualified. Because prefix of S3 bucket is always "",
-- client path acts like a noop.
module Data.ClientPath
  ( ClientPath(..)
  , RawClientPath(..)
  , AbsPath(..)
  , (<./>)
  , newAbsPath
  , toClientPath
  , fromClientPath
  , toRawClientPath
  , fromRawClientPath
  )
  where

import Data.Aeson (ToJSON(..))
import Data.List (stripPrefix)
import Network.URI.Encode qualified as URI.Encode
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import System.FilePath ((</>), normalise, isAbsolute)
import Text.Debug (Debug(..))
import Data.Hashable (Hashable)
import Data.Coerce (coerce)


newtype AbsPath = AbsPath { unAbsPath :: FilePath }
  deriving (Show, Eq)
  deriving newtype (Debug, Hashable, ToJSON)


(<./>) :: AbsPath -> AbsPath -> AbsPath
AbsPath a1 <./> AbsPath a2 = AbsPath (a1 </> a2)


newAbsPath :: FilePath -> Maybe AbsPath
newAbsPath path
  | isAbsolute path = Just (AbsPath path)
  | otherwise       = Nothing


-- | Filepath without the prefix part. The path is percent encoded safe to show in the frontend.
newtype ClientPath = ClientPath { unClientPath :: FilePath }
  deriving (Show, Eq)
  deriving newtype (Semigroup, Monoid, Debug)


-- | ClientPath but not percent encoded
newtype RawClientPath = RawClientPath { unRawClientPath :: FilePath }
  deriving (Show, Eq)
  deriving newtype (Semigroup, Monoid, Debug)


instance ToHttpApiData ClientPath where
  toUrlPiece (ClientPath p) = toUrlPiece p


instance FromHttpApiData ClientPath where
  parseUrlPiece p = ClientPath <$> parseUrlPiece p


instance ToJSON ClientPath where
  toJSON = toJSON . toUrlPiece


-- | Convert a file path into a ClientPath.
toClientPath :: AbsPath -> AbsPath -> ClientPath
toClientPath (AbsPath prefix) (AbsPath path)=
  let RawClientPath rcp = toRawClientPath prefix path
   in ClientPath (URI.Encode.encode rcp)


fromClientPath :: AbsPath -> ClientPath -> AbsPath
fromClientPath prefix (ClientPath cp) =
  let decoded = URI.Encode.decode cp
   in AbsPath (fromRawClientPath (coerce prefix) (RawClientPath decoded))


-- | Remove the prefix part from the path, don't encode any characters.
--   prefix should starts with '/'. If not toRawClientPath will append one before stripping.
--   The path returned is guaranteed not start with '/'
toRawClientPath :: FilePath -> FilePath -> RawClientPath
toRawClientPath prefix path = do
  RawClientPath (normalise (ensureUnslash (removePrefix (ensureSlash prefix) path)))


fromRawClientPath :: FilePath -> RawClientPath -> FilePath
fromRawClientPath prefix (RawClientPath cp) = normalise (prefix </> ensureUnslash cp)


removePrefix :: FilePath -> FilePath -> FilePath
removePrefix prefix path =
  case stripPrefix (normalise prefix) (normalise path) of
    Just removed -> removed
    Nothing -> path


ensureSlash :: FilePath -> FilePath
ensureSlash path =
  case path of
    '/':_ -> path
    _     -> '/':path


ensureUnslash :: FilePath -> FilePath
ensureUnslash path =
  case path of
    '/':rest -> rest
    _        -> path
