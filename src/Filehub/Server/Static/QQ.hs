{-# LANGUAGE TemplateHaskell #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2026-present Jinyang yao
--
-- The template haskell used here slow down the language server drastically.
-- We pull it into its own module to avoid bad dev performance.


module Filehub.Server.Static.QQ (staticFiles) where

import Data.ByteString (ByteString)
import Data.FileEmbed qualified as FileEmbed
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Filehub.Orphan ()
import Prelude hiding (init, readFile)


-- | Static files are embeded into the final excutable. The key is the path of the file.
--   e.g main.js -> (data/filehub/main.js)
staticFiles :: Map FilePath ByteString
staticFiles = M.fromList
  $(FileEmbed.embedDir "data/filehub")
