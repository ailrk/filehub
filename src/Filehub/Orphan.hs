{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- The dumpster of all orphan instances.
module Filehub.Orphan () where
import Servant (ToHttpApiData, URI)
import Servant.API (ToHttpApiData(..))
import Data.Text qualified as Text
import Network.URI qualified as URI
import Data.Aeson (FromJSON (..), withText)


instance ToHttpApiData URI where
  toUrlPiece uri = Text.pack (URI.uriToString id uri "")


instance FromJSON URI where
  parseJSON = withText "URI" \t ->
    case URI.parseURI (Text.unpack t) of
      Just u  -> pure u
      Nothing -> fail ("Invalid URI: " <> Text.unpack t)
