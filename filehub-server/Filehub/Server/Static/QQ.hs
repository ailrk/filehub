{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2026-present Jinyang yao
--
-- The template haskell used here slow down the language server drastically.
-- We pull it into its own module to avoid bad dev performance.


module Filehub.Server.Static.QQ (staticFiles) where

import Filehub.Prelude
import Data.FileEmbed qualified as FileEmbed
import Filehub.Orphan ()
import Prelude hiding (init, readFile)

#ifdef DEBUG
import GHC.IO.Unsafe (unsafePerformIO)
#endif


-- | Static files are embeded into the final excutable. The key is the path of the file.
--   e.g main.js -> (data/filehub/main.js)
staticFiles :: Map FilePath ByteString

-- It's very slow to run template haskell with repl.
#ifdef DEBUG
staticFiles = unsafePerformIO $ map_fromList <$> FileEmbed.getDir "data/filehub"

#else
staticFiles = map_fromList $(FileEmbed.embedDir "data/filehub")
#endif



