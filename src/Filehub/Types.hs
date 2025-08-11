{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Filehub.Types
  ( LoginForm(..)
  , Session(..)
  , TargetSessionData(..)
  , Selected(..)
  , CopyState(..)
  , ControlPanelState(..)
  , SessionId(..)
  , SessionPool(..)
  , Env(..)
  , Resolution(..)
  , Display(..)
  , TargetId(..)
  , Target(..)
  , FileContent(..)
  , File(..)
  , ClientPath(..)
  , RawClientPath(..)
  , SortFileBy(..)
  , SearchWord(..)
  , NewFile(..)
  , NewFolder(..)
  , UpdatedFile(..)
  , Theme(..)
  , FilehubEvent(..)
  , Resource(..)
  , Manifest
  )
  where


import Data.Text (Text)
import Data.Text.Lazy.Encoding qualified as LText
import Data.Aeson (ToJSON (..), (.=), Value)
import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Platform ()
import Servant
    ( ToHttpApiData(..),
      Accept (..),
      MimeRender )
import Web.FormUrlEncoded (FromForm (..), parseUnique, ToForm (..))
import Servant.API (MimeRender(..))
import Filehub.Target.Types (Target (..))
import Filehub.Target.Types.TargetId (TargetId(..))
import Filehub.ClientPath (ClientPath(..), RawClientPath(..))
import Filehub.File (File(..), FileContent(..))
import Filehub.Theme (Theme(..))
import Filehub.Display (Display(..), Resolution(..))
import Filehub.Sort (SortFileBy(..))
import Filehub.Selected.Types (Selected(..))
import Filehub.Copy.Types (CopyState(..))
import Filehub.Session.Types (SessionId(..), Session(..), TargetSessionData(..))
import Filehub.SessionPool.Types (SessionPool(..))
import Filehub.Env.Types (Env(..))
import GHC.IsList (fromList)


data LoginForm = LoginForm
  { username :: Text
  , password :: Text
  } deriving (Show)


instance FromForm LoginForm where
  fromForm f = LoginForm
    <$> parseUnique "username" f
    <*> parseUnique "password" f


instance ToForm LoginForm where
  toForm (LoginForm username password) =
      fromList
        [ ("username", toQueryParam username)
        , ("password", toQueryParam password)
        ]


data ControlPanelState
  = ControlPanelDefault
  | ControlPanelSelecting
  | ControlPanelCopied


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
  { url :: RawClientPath
  , mimetype :: Text
  }
  deriving (Show, Eq)


instance ToJSON Resource where
  toJSON (Resource { url = RawClientPath path, mimetype }) = toJSON [ toJSON path , toJSON mimetype ]


data FilehubEvent
  = ViewerInited [Resource] Int -- Update image list and show the viewer
  | TargetChanged
  | TableSorted
  | DirChanged
  | LayoutChanged
  | ThemeChanged
  | Canceled -- Action canceled
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
  toJSON LayoutChanged = Aeson.object [ "LayoutChanged" .= Aeson.object [] ]
  toJSON ThemeChanged = Aeson.object [ "ThemeChanged" .= Aeson.object [] ]
  toJSON Canceled = Aeson.object [ "Canceled" .= Aeson.object [] ]


instance ToHttpApiData FilehubEvent where
  toUrlPiece v = (v & Aeson.encode & LText.decodeUtf8) ^. strict


data Manifest


instance Accept Manifest where
  contentType _ = "application/manifest+json"


instance MimeRender Manifest Value where
  mimeRender _ = Aeson.encode
