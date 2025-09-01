{-# OPTIONS_GHC -Wno-orphans #-}

module Filehub.Orphan where
import Servant (ToHttpApiData, URI)
import Servant.API (ToHttpApiData(..))
import Data.Text qualified as Text
import Network.URI qualified as URI


instance ToHttpApiData URI where
  toUrlPiece uri = Text.pack $ URI.uriToString id uri ""
