-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements sorting strategies for the table. Directories will be
-- sorted with higher priority by default.
module Filehub.Sort (SortFileBy(..), sortFiles) where

import Filehub.Prelude
import Data.ClientPath (AbsPath(..))
import Data.File (File(..), FileType(..), FileInfo)
import Data.List ( sortOn )
import Data.Ord (Down(..))
import Servant (ToHttpApiData(..), FromHttpApiData(..))
import System.FilePath ( takeFileName )
import Text.Debug (Debug (..))


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


byFileNamewithDirFirst :: FileInfo -> (Down FileType, String)
byFileNamewithDirFirst file =
  ( Down file.content              -- Regular < Dir, so Down Dir < Down Regular → Dir sorts first
  , coerce takeFileName file.path
  )


sortFiles :: SortFileBy -> [FileInfo] -> [FileInfo]
sortFiles ByNameUp       = sortOn byFileNamewithDirFirst
sortFiles ByNameDown     = reverse . sortFiles ByNameUp
sortFiles ByModifiedUp   = sortOn (\f -> (f.content, f.mtime))
sortFiles ByModifiedDown = reverse . sortFiles ByModifiedUp
sortFiles BySizeUp       = sortOn (\f -> (f.content, f.size))
sortFiles BySizeDown     = reverse . sortFiles BySizeUp
