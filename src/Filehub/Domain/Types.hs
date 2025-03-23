{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Filehub.Domain.Types
  ( FilehubError(..)
  , FileContent(..)
  , File(..)
  , ClientPath(..)
  , SortFileBy(..)
  , SearchWord(..)
  , NewFile(..)
  , NewFolder(..)
  , UpdatedFile(..)
  , Resource(..)
  , Viewer(..)
  , Theme(..)
  )
  where

import GHC.Generics
import Data.Text (Text)
import Web.FormUrlEncoded (FromForm (..), parseUnique)
import Servant (ToHttpApiData(..), FromHttpApiData (..))
import Data.Time (UTCTime)
import Network.Mime (MimeType)
import Network.URI.Encode qualified as URI.Encode
import Lens.Micro.Platform ()
import Data.Text qualified as Text
import Lens.Micro
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Text.Lazy.Encoding qualified as LText


data FilehubError
  = FileExists
  | InvalidPath
  | InvalidDir
  | InvalidSession
  | InternalError
  deriving Show


instance ToHttpApiData FilehubError where
  toUrlPiece FileExists = Text.pack $ show FileExists
  toUrlPiece InvalidPath = Text.pack $ show InvalidPath
  toUrlPiece InvalidDir = Text.pack $ show InvalidDir
  toUrlPiece InvalidSession = Text.pack $ show InvalidSession
  toUrlPiece InternalError = Text.pack $ show InternalError


data FileContent
  = Content
  | Dir (Maybe [File])
  deriving (Show, Eq, Generic)


data File = File
  { path :: FilePath
  , atime :: UTCTime
  , mtime :: UTCTime
  , size :: Integer
  , mimetype :: MimeType
  , content :: FileContent
  }
  deriving (Show, Eq, Generic)


instance Ord File where
  compare a b = compare a.path b.path


-- | Filepath without the root part. The path is safe to show in the frontend.
newtype ClientPath = ClientPath { unClientPath :: FilePath }
  deriving (Show, Eq, Semigroup, Monoid)


instance ToHttpApiData ClientPath where
  toUrlPiece (ClientPath p) = toUrlPiece p


instance FromHttpApiData ClientPath where
  parseUrlPiece p = ClientPath <$> parseUrlPiece (URI.Encode.decodeText p)


data SortFileBy
  = ByName
  | ByModified
  | BySize
  deriving (Show, Eq)


instance ToHttpApiData SortFileBy where
  toUrlPiece ByName = "name"
  toUrlPiece ByModified = "modified"
  toUrlPiece BySize = "size"


instance FromHttpApiData SortFileBy where
  parseUrlPiece "name" = pure ByName
  parseUrlPiece "modified" = pure ByModified
  parseUrlPiece "size" = pure BySize
  parseUrlPiece _ = Left "Unknown order"


newtype SearchWord = SearchWord Text deriving (Show, Eq, Generic)
instance FromForm SearchWord where fromForm f = SearchWord <$> parseUnique "search" f


newtype NewFile = NewFile Text deriving (Show, Eq, Generic)
instance FromForm NewFile where fromForm f = NewFile <$> parseUnique "new-file" f


newtype NewFolder = NewFolder Text deriving (Show, Eq, Generic)
instance FromForm NewFolder where fromForm f = NewFolder <$> parseUnique "new-folder" f


data UpdatedFile = UpdatedFile
  { clientPath :: ClientPath
  , content :: Text
  }
  deriving (Show, Eq, Generic)
instance FromForm UpdatedFile where
  fromForm f = do
    path <- parseUnique "path" f
    content <- parseUnique "content" f
    pure (UpdatedFile path content)


data Resource = Resource
  { url :: Text
  , mimetype :: Text
  }
  deriving (Show, Eq)


instance ToJSON Resource where
  toJSON (Resource { url, mimetype }) =
    Aeson.object
      [ "url" .= toJSON url
      , "mimetype" .= mimetype
      ]


data Viewer = InitViewer [Resource] Int -- Update image list and show the viewer
  deriving (Show)


instance ToJSON Viewer where
  toJSON (InitViewer res index) =
    Aeson.object
      [ "InitViewer" .= Aeson.object
          [ "resources" .= toJSON res
          , "index" .= toJSON index
          ]
      ]


instance ToHttpApiData Viewer where
  toUrlPiece v = (v & Aeson.encode & LText.decodeUtf8) ^. strict



data Theme = Dark1 | Dark2 | Dark3 | Light1 | Light2 | Light3

instance Show Theme where
  show = \case
    Dark1 -> "dark1"
    Dark2 -> "dark2"
    Dark3 -> "dark3"
    Light1 -> "light1"
    Light2 -> "light2"
    Light3 -> "light3"


instance Read Theme where
  readsPrec _ s = do
    let theme =
          case s of
          "dark1" -> Dark1
          "dark2" -> Dark2
          "dark3" -> Dark3
          "light1" -> Light1
          "light2" -> Light2
          "light3" -> Light3
          _ -> Dark1
    pure (theme, "")
