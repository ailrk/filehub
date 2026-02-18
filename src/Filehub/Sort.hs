-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements sorting strategies for the table. Directories will be
-- sorted with higher priority by default.
module Filehub.Sort (SortFileBy(..), sortFiles) where

import Data.File (File(..), FileType(..), FileInfo)
import Data.List ( sortOn )
import System.FilePath ( takeFileName )
import Data.ClientPath (AbsPath(..))
import Servant (ToHttpApiData(..), FromHttpApiData(..))
import Text.Debug (Debug (..))
import Data.Coerce (coerce)


data SortFileBy
  = ByNameUp
  | ByNameDown
  | ByModifiedUp
  | ByModifiedDown
  | BySizeUp
  | BySizeDown
  deriving (Show, Eq)


instance Debug SortFileBy where debug = show


instance ToHttpApiData SortFileBy where
  toUrlPiece ByNameUp       = "nameUp"
  toUrlPiece ByNameDown     = "nameDown"
  toUrlPiece ByModifiedUp   = "modifiedUp"
  toUrlPiece ByModifiedDown = "modifiedDown"
  toUrlPiece BySizeUp       = "sizeUp"
  toUrlPiece BySizeDown     = "sizeDown"


instance FromHttpApiData SortFileBy where
  parseUrlPiece "nameUp"       = pure ByNameUp
  parseUrlPiece "nameDown"     = pure ByNameDown
  parseUrlPiece "modifiedUp"   = pure ByModifiedUp
  parseUrlPiece "modifiedDown" = pure ByModifiedDown
  parseUrlPiece "sizeUp"       = pure BySizeUp
  parseUrlPiece "sizeDown"     = pure BySizeDown
  parseUrlPiece _              = Left "Unknown order"


byFileNamewithDirFirst :: FileInfo -> String
byFileNamewithDirFirst file = do
  let pre = case file.content of
              Regular -> '1'
              Dir     -> '0'
  let name = coerce takeFileName file.path :: String
  pre : name


sortFiles :: SortFileBy -> [FileInfo] -> [FileInfo]
sortFiles ByNameUp       = sortOn byFileNamewithDirFirst
sortFiles ByNameDown     = reverse . sortFiles ByNameUp
sortFiles ByModifiedUp   = sortOn (.mtime)
sortFiles ByModifiedDown = reverse . sortFiles ByModifiedUp
sortFiles BySizeUp       = sortOn (.size)
sortFiles BySizeDown     = reverse . sortFiles BySizeUp
