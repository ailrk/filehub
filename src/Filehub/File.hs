{-# LANGUAGE DeriveGeneric #-}
module Filehub.File where

import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Network.Mime (MimeType)


data FileContent
  = Content
  | Dir (Maybe [File])
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
