-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements Sorting strategy for the table. By default
-- directories will be sorted with higher priority.

module Filehub.Sort (SortFileBy(..), sortFiles) where

import Data.List ( sortOn )
import System.FilePath ( takeFileName )
import Servant (ToHttpApiData(..), FromHttpApiData(..))
import Filehub.File (File(..), FileContent (..))


data SortFileBy
  = ByNameUp
  | ByNameDown
  | ByModifiedUp
  | ByModifiedDown
  | BySizeUp
  | BySizeDown
  deriving (Show, Eq)


instance ToHttpApiData SortFileBy where
  toUrlPiece ByNameUp = "nameUp"
  toUrlPiece ByNameDown = "nameDown"
  toUrlPiece ByModifiedUp = "modifiedUp"
  toUrlPiece ByModifiedDown = "modifiedDown"
  toUrlPiece BySizeUp = "sizeUp"
  toUrlPiece BySizeDown = "sizeDown"


instance FromHttpApiData SortFileBy where
  parseUrlPiece "nameUp" = pure ByNameUp
  parseUrlPiece "nameDown" = pure ByNameDown
  parseUrlPiece "modifiedUp" = pure ByModifiedUp
  parseUrlPiece "modifiedDown" = pure ByModifiedDown
  parseUrlPiece "sizeUp" = pure BySizeUp
  parseUrlPiece "sizeDown" = pure BySizeDown
  parseUrlPiece _ = Left "Unknown order"


byFileNamewithDirFirst :: File -> String
byFileNamewithDirFirst file = do
  let pre = case file.content of
              Content -> '1'
              Dir _ -> '0'
  let name = takeFileName file.path :: String
  pre : name


sortFiles :: SortFileBy -> [File] -> [File]
sortFiles ByNameUp = sortOn byFileNamewithDirFirst
sortFiles ByNameDown = reverse . sortFiles ByNameUp
sortFiles ByModifiedUp = sortOn (.mtime)
sortFiles ByModifiedDown = reverse . sortFiles ByModifiedUp
sortFiles BySizeUp = sortOn (.size)
sortFiles BySizeDown = reverse . sortFiles BySizeUp
