{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Target.Types.TargetId (TargetId(..)) where

import Data.Hashable (Hashable)
import Data.UUID (UUID)
import Servant
    ( FromHttpApiData(..),
      ToHttpApiData(..),
      ToHttpApiData(..),
      FromHttpApiData(..) )
import Network.URI.Encode qualified as URI.Encode

newtype TargetId = TargetId UUID deriving (Show, Eq, Ord, Hashable)

instance ToHttpApiData TargetId where
  toUrlPiece (TargetId p) = toUrlPiece p


instance FromHttpApiData TargetId where
  parseUrlPiece p = TargetId <$> parseUrlPiece (URI.Encode.decodeText p)



