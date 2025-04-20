{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Filehub.Domain.Types
  ( FileContent(..)
  , File(..)
  , ClientPath(..)
  , SortFileBy(..)
  , SearchWord(..)
  , NewFile(..)
  , NewFolder(..)
  , UpdatedFile(..)
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


data FileContent
  = Content
  | Dir (Maybe [File])
  deriving (Show, Eq, Generic)


data File = File
  { path :: FilePath
  , atime :: Maybe UTCTime
  , mtime :: Maybe UTCTime
  , size :: Maybe Integer
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


data Theme = Dark | Light

instance Show Theme where
  show = \case
    Dark -> "dark"
    Light -> "light"


instance Read Theme where
  readsPrec _ s = do
    let theme =
          case s of
          "dark" -> Dark
          "light" -> Light
          _ -> Dark
    pure (theme, "")
