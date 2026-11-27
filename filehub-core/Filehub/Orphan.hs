{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- The dumpster of all orphan instances.
module Filehub.Orphan () where
import Filehub.Prelude
import Servant (ToHttpApiData, URI)
import Servant.API (ToHttpApiData(..))
import Network.URI qualified as URI


instance ToHttpApiData URI where
  toUrlPiece uri = txt_pack (URI.uriToString id uri "")
