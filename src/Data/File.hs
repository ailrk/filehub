{-# LANGUAGE DeriveGeneric #-}
module Data.File
  ( FileType(..)
  , FileContent(..)
  , File(..)
  , FileInfo
  , FileWithContent
  , defaultFileInfo
  , defaultFileWithContent
  , withContent
  , extractFileInfo
  )
  where

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
  = FileContentRaw ByteString
  -- ^ Raw bytestring content
  | FileContentConduit (ConduitT () ByteString (ResourceT IO) ())
  -- ^ Content is ready as conduit
  | FileContentDir [FileWithContent]
  -- ^ Directory structure
  | FileContentNull
  -- ^ No content


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


defaultFileInfo :: FileInfo
defaultFileInfo =
  File
    { path      = ""
    , atime     = Nothing
    , mtime     = Nothing
    , size      = Nothing
    , mimetype  = "application/octet-stream"
    , content   = Regular
    }


defaultFileWithContent :: FileWithContent
defaultFileWithContent =
  File
    { path      = ""
    , atime     = Nothing
    , mtime     = Nothing
    , size      = Nothing
    , mimetype  = "application/octet-stream"
    , content   = FileContentNull
    }


withContent :: FileInfo -> FileContent -> FileWithContent
withContent file content =
  File
    { path      = file.path
    , atime     = file.atime
    , mtime     = file.mtime
    , size      = file.size
    , mimetype  = file.mimetype
    , content   = content
    }


extractFileInfo :: FileWithContent -> FileInfo
extractFileInfo file =
  File
    { path     = file.path
    , atime    = file.atime
    , mtime    = file.mtime
    , size     = file.size
    , mimetype = file.mimetype
    , content  = case file.content of
                   FileContentDir _ -> Dir
                   _ -> Regular
    }
