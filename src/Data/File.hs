{-# LANGUAGE DeriveGeneric #-}
module Data.File where

import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Network.Mime (MimeType)


-- | `FileContent` represents the tree structure of a folder without pulling in
-- any content. To read content from a file, use `Storage.read` or `Storage.readStream`
data FileContent
  = Content
  | Dir
  deriving (Show, Eq, Generic)


data File = File
  { path     :: FilePath -- absolute path
  , atime    :: Maybe UTCTime
  , mtime    :: Maybe UTCTime
  , size     :: Maybe Integer
  , mimetype :: MimeType
  , content  :: FileContent
  }
  deriving (Show, Eq, Generic)


instance Ord File where
  compare a b = compare a.path b.path
