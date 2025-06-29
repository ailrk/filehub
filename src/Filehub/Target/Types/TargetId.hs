{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Target.Types.TargetId where

import Data.UUID (UUID)
import Data.Hashable (Hashable)
import Servant (ToHttpApiData (..), FromHttpApiData (..))
import Network.URI.Encode qualified as URI.Encode


newtype TargetId = TargetId UUID deriving (Show, Eq, Ord, Hashable)


instance ToHttpApiData TargetId where
  toUrlPiece (TargetId p) = toUrlPiece p


instance FromHttpApiData TargetId where
  parseUrlPiece p = TargetId <$> parseUrlPiece (URI.Encode.decodeText p)
