{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Target.Types.TargetId where

import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Hashable (Hashable)
import Servant (ToHttpApiData (..), FromHttpApiData (..))
import Network.URI.Encode qualified as URI.Encode
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder


newtype TargetId = TargetId UUID deriving (Show, Eq, Ord, Hashable)


targetIdBuilder :: TargetId -> Builder
targetIdBuilder (TargetId targetId) =  Builder.byteString . UUID.toASCIIBytes $ targetId


instance ToHttpApiData TargetId where
  toUrlPiece (TargetId p) = toUrlPiece p


instance FromHttpApiData TargetId where
  parseUrlPiece p = TargetId <$> parseUrlPiece (URI.Encode.decodeText p)
