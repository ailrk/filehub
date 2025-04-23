{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Filehub.Types
  ( Session(..)
  , TargetSessionData(..)
  , Selected(..)
  , CopyState(..)
  , SessionId(..)
  , SessionPool(..)
  , Env(..)
  , TargetId(..)
  , Target(..)
  , S3Target(..)
  , FileTarget(..)
  , FileContent(..)
  , File(..)
  , ClientPath(..)
  , SortFileBy(..)
  , SearchWord(..)
  , NewFile(..)
  , NewFolder(..)
  , UpdatedFile(..)
  , Theme(..)
  , FilehubEvent(..)
  , Resource(..)
  )
  where


import Amazonka qualified
import Control.Concurrent.Timer qualified as Timer
import Data.HashTable.IO (BasicHashTable)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Lazy.Encoding qualified as LText
import Data.Time (UTCTime, NominalDiffTime)
import Data.UUID (UUID)
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Platform ()
import Network.Mime (MimeType)
import Network.URI.Encode qualified as URI.Encode
import Servant
    ( FromHttpApiData(..),
      ToHttpApiData(..),
      ToHttpApiData(..),
      FromHttpApiData(..) )
import Web.FormUrlEncoded (FromForm (..), parseUnique, parseAll)


newtype SessionId = SessionId UUID
  deriving (Show, Eq, Ord, Hashable)


data Session = Session
  { sessionId :: SessionId
  , expireDate :: UTCTime
  , targets :: [TargetSessionData]
  , index :: Int
  }
  deriving (Generic)


instance Eq Session where
  a == b = a.sessionId == b.sessionId


data TargetSessionData = TargetSessionData
  { currentDir :: FilePath
  , sortedFileBy :: SortFileBy
  , copyState :: CopyState
  , selected :: Selected
  }
  deriving (Generic)


data Selected
  = Selected ClientPath [ClientPath]
  | NoSelection
  deriving (Show, Eq)


instance FromForm Selected where
  fromForm f = do
    selected <- parseAll "selected" f
    case selected of
      [] -> pure NoSelection
      x:xs->  pure $ Selected x xs


data SessionPool = SessionPool
  { pool :: BasicHashTable SessionId Session
  , gc :: Timer.TimerIO
  -- ^ garbage collector, periodically clean up expired sessions.
  }


type From = Target
type To = Target


data CopyState
  -- | Select files to copy
  = CopySelect [(From, [File])] [File]
  -- | Ready to paste
  | CopySelected [(From, [File])]
  -- | Start pasting files to target path
  | Paste [(From, [File])] To FilePath
  -- | The previous copy paste has been completed
  | CopyFinished
  -- | No copy paste action being performed at the moment.
  | NoCopyPaste


newtype TargetId = TargetId UUID deriving (Show, Eq, Ord, Hashable)


instance ToHttpApiData TargetId where
  toUrlPiece (TargetId p) = toUrlPiece p


instance FromHttpApiData TargetId where
  parseUrlPiece p = TargetId <$> parseUrlPiece (URI.Encode.decodeText p)



data Target
  = S3Target S3Target
  | FileTarget FileTarget
  deriving (Generic)


instance Eq Target where
  S3Target a == S3Target b = a.targetId == b.targetId
  FileTarget a == FileTarget b = a.targetId == b.targetId
  _ == _ = False


data S3Target = S3Target_
  { targetId :: TargetId
  , bucket :: Text
  , env :: Amazonka.Env
  }
  deriving (Generic)


data FileTarget = FileTarget_
  { targetId :: TargetId
  , targetName :: Maybe Text
  , root :: FilePath
  }
  deriving (Show, Eq, Generic)


data Env = Env
  { port :: !Int
  , theme :: Theme
  , dataDir :: !FilePath
  , sessionPool :: SessionPool
  , sessionDuration :: NominalDiffTime
  , targets :: [Target]
  }


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


data FilehubEvent
  = ViewerInited [Resource] Int -- Update image list and show the viewer
  | TargetChanged
  | TableSorted
  | DirChanged
  deriving (Show)


instance ToJSON FilehubEvent where
  toJSON (ViewerInited res index) =
    Aeson.object
      [ "ViewerInited" .= Aeson.object
          [ "resources" .= toJSON res
          , "index" .= toJSON index
          ]
      ]
  toJSON TargetChanged = Aeson.object [ "TargetChanged" .= Aeson.object [] ]
  toJSON TableSorted = Aeson.object [ "TableSorted" .= Aeson.object [] ]
  toJSON DirChanged = Aeson.object [ "DirChanged" .= Aeson.object [] ]


instance ToHttpApiData FilehubEvent where
  toUrlPiece v = (v & Aeson.encode & LText.decodeUtf8) ^. strict
