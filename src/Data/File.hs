{-# LANGUAGE DeriveGeneric #-}
module Data.File where

import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Network.Mime (MimeType)
import Data.ByteString (ByteString)
import Conduit (ConduitT, ResourceT)


data FileType
  = Regular
  | Dir
  deriving (Show, Eq, Generic)


data FileContent
  = FileContentRaw     ByteString
  | FileContentConduit (ConduitT () ByteString (ResourceT IO) ())
  | FileContentDir



data File a = File
  { path     :: FilePath -- absolute path
  , atime    :: Maybe UTCTime
  , mtime    :: Maybe UTCTime
  , size     :: Maybe Integer
  , mimetype :: MimeType
  , content  :: a
  }
  deriving (Eq)


type FileInfo        = File FileType
type FileWithContent = File FileContent
