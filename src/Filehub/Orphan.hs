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


instance ToHttpApiData URI where
  toUrlPiece uri = Text.pack (URI.uriToString id uri "")
