module Filehub.Layout where

import Servant (ToHttpApiData(..), FromHttpApiData(..))

data Layout
  = ThumbnailLayout
  | ListLayout
  deriving (Show, Eq)


instance ToHttpApiData Layout where
  toUrlPiece ThumbnailLayout = "ThumbnailLayout"
  toUrlPiece ListLayout      = "ListLayout"


instance FromHttpApiData Layout where
  parseUrlPiece "ThumbnailLayout" = pure ThumbnailLayout
  parseUrlPiece "ListLayout"      = pure ListLayout
  parseUrlPiece _                 = Left "Unknown layout"
