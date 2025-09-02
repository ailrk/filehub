module Filehub.Types where

import Data.Time (UTCTime)
import Network.Mime (MimeType)
import {-# SOURCE #-} Filehub.Target.Types (Target)


data File = File
  { path     :: FilePath
  , atime    :: Maybe UTCTime
  , mtime    :: Maybe UTCTime
  , size     :: Maybe Integer
  , mimetype :: MimeType
  , content  :: FileContent
  }


data FileContent
  = Content
  | Dir (Maybe [File])


data Layout
  = ThumbnailLayout
  | ListLayout


data CopyState
  = CopySelected [(Target, [File])]
  | Paste [(Target, [File])]
  | NoCopyPaste


data Selected
