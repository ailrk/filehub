{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Where you put your types when you don't know where they belongs to.
module Filehub.Types
  ( LoginForm(..)
  , Session(..)
  , TargetSessionData(..)
  , Selected(..)
  , CopyState(..)
  , ControlPanelState(..)
  , Layout(..)
  , SessionId(..)
  , Env(..)
  , Resolution(..)
  , Display(..)
  , ClientPath(..)
  , RawClientPath(..)
  , SortFileBy(..)
  , SearchWord(..)
  , NewFile(..)
  , NewFolder(..)
  , UpdatedFile(..)
  , MoveFile(..)
  , Theme(..)
  , FilehubEvent(..)
  , OpenTarget (..)
  , UIComponent(..)
  , Resource(..)
  , Manifest
  )
  where

import Data.Aeson (ToJSON (..), (.=), Value)
import Data.Aeson qualified as Aeson
import Data.ClientPath (ClientPath(..), RawClientPath(..))
import Data.File (FileInfo)
import Data.Text (Text)
import Data.Text.Lazy.Encoding qualified as LText
import Filehub.Display (Display(..), Resolution(..))
import Filehub.Env (Env(..))
import Filehub.Session.Types (SessionId(..), Session(..), TargetSessionData(..))
import Filehub.Sort (SortFileBy(..))
import Filehub.Theme (Theme(..))
import GHC.Generics (Generic)
import GHC.IsList (fromList)
import Lens.Micro
import Lens.Micro.Platform ()
import Servant ( ToHttpApiData(..), FromHttpApiData(..), Accept (..), MimeRender )
import Servant.API (MimeRender(..))
import Target.Types (Target (..))
import Web.FormUrlEncoded (FromForm (..), parseUnique, ToForm (..), parseAll)


-- | Simple Auth login form
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


-- | The state of the control panel.
data ControlPanelState
  = ControlPanelDefault
  | ControlPanelSelecting
  | ControlPanelCopied
  deriving (Show, Eq)


-- | The table layout of a session
data Layout
  = ThumbnailLayout
  | ListLayout
  deriving (Show, Eq)


instance ToHttpApiData Layout where
  toUrlPiece ThumbnailLayout = "ThumbnailLayout"
  toUrlPiece ListLayout      = "ListLayout"


instance FromHttpApiData Layout where
  parseUrlPiece "ThumbnailLayout" = pure ThumbnailLayout
  parseUrlPiece "ListLayout"      = pure ListLayout
  parseUrlPiece _                 = Left "Unknown layout"


-- | State machine reprents the copy and paste process.
data CopyState
 -- | Ready to paste
  = CopySelected [(Target, [FileInfo])]
  -- | Start pasting files to target path
  | Paste [(Target, [FileInfo])]
  -- | No copy paste action being performed at the moment.
  | NoCopyPaste


data Selected
  = Selected ClientPath [ClientPath] -- non empty list
  | NoSelection
  deriving (Show, Eq)


instance FromForm Selected where
  fromForm f = do
    selected <- parseAll "selected" f
    case selected of
      []   -> pure NoSelection
      x:xs -> pure $ Selected x xs


newtype SearchWord = SearchWord Text deriving (Show, Eq, Generic)
instance FromForm SearchWord where fromForm f = SearchWord <$> parseUnique "search" f


newtype NewFile = NewFile Text deriving (Show, Eq, Generic)
instance FromForm NewFile where fromForm f = NewFile <$> parseUnique "new-file" f


newtype NewFolder = NewFolder Text deriving (Show, Eq, Generic)
instance FromForm NewFolder where fromForm f = NewFolder <$> parseUnique "new-folder" f


data MoveFile = MoveFile [ClientPath] ClientPath deriving (Show, Eq)
instance FromForm MoveFile where fromForm f = MoveFile <$> parseAll "src" f <*> parseUnique "tgt" f


data UpdatedFile = UpdatedFile
  { clientPath :: ClientPath
  , content    :: Text
  }
  deriving (Show, Eq, Generic)
instance FromForm UpdatedFile where
  fromForm f = do
    path    <- parseUnique "path" f
    content <- parseUnique "content" f
    pure (UpdatedFile path content)


data Resource = Resource
  { url :: RawClientPath
  , mimetype :: Text
  }
  deriving (Show, Eq)


instance ToJSON Resource where
  toJSON (Resource { url = RawClientPath path, mimetype }) = toJSON [ toJSON path , toJSON mimetype ]


data UIComponent
  = UIComponentView
  | UIComponentSideBar
  | UIComponentContronPanel
  | UIComponentIndex
  deriving (Show)


instance ToJSON UIComponent where
  toJSON = toJSON . show


instance ToHttpApiData UIComponent where
  toUrlPiece v = (v & Aeson.encode & LText.decodeUtf8) ^. strict


instance FromHttpApiData UIComponent where
  parseUrlPiece "UIComponentView"         = pure UIComponentView
  parseUrlPiece "UIComponentSideBar"      = pure UIComponentSideBar
  parseUrlPiece "UIComponentContronPanel" = pure UIComponentContronPanel
  parseUrlPiece "UIComponentIndex"        = pure UIComponentIndex
  parseUrlPiece _                         = Left "unknown ui component"


-- | Filehub events that delivers to the frontend via HX-Trigger. By default these events are
-- just ignored in the frontend. To handle an event, add a corresponding event handler in `ui.ts`
-- When adding a new event, as long as you don't plan to handle the event yet, you don't need to
-- touch the frontend code.
data FilehubEvent
  = ViewerInited [Resource] Int -- Update image list and show the viewer
  | TargetChanged
  | TableSorted
  | DirChanged
  | LayoutChanged
  | ThemeChanged
  | LocaleChanged
  | FileMoved
  | Canceled -- Action canceled
  | Opened OpenTarget ClientPath -- load a resource into tab/window/iframe. Hook  for window.open
  | UIComponentReloaded UIComponent
  | SSEStarted
  | Dummy Text -- dummy event for testing
  deriving (Show)


instance ToJSON FilehubEvent where
  toJSON (ViewerInited res index) =
    Aeson.object
      [ "ViewerInited" .= Aeson.object
          [ "resources" .= toJSON res
          , "index"     .= toJSON index
          ]
      ]
  toJSON TargetChanged  = Aeson.object [ "TargetChanged" .= Aeson.object [] ]
  toJSON TableSorted    = Aeson.object [ "TableSorted"   .= Aeson.object [] ]
  toJSON DirChanged     = Aeson.object [ "DirChanged"    .= Aeson.object [] ]
  toJSON LayoutChanged  = Aeson.object [ "LayoutChanged" .= Aeson.object [] ]
  toJSON ThemeChanged   = Aeson.object [ "ThemeChanged"  .= Aeson.object [] ]
  toJSON LocaleChanged  = Aeson.object [ "LocaleChanged" .= Aeson.object [] ]
  toJSON FileMoved      = Aeson.object [ "FileMoved"     .= Aeson.object [] ]
  toJSON Canceled       = Aeson.object [ "Canceled"      .= Aeson.object [] ]
  toJSON (Opened target path) =
    Aeson.object
      [ "Opened" .= Aeson.object
          [ "path" .= toJSON path
          , "tgt"  .= toJSON target
          ]
      ]
  toJSON (UIComponentReloaded comp) =
    Aeson.object
      [ "UIComponentReloaded" .= Aeson.object
          [ "component" .= toJSON comp
          ]
      ]
  toJSON SSEStarted = Aeson.object [ "SSEStarted" .= Aeson.object [] ]
  toJSON (Dummy t)  = Aeson.object [ "Dummy"      .= Aeson.object [ "msg" .= t ] ]


instance ToHttpApiData FilehubEvent where
  toUrlPiece v = (v & Aeson.encode & LText.decodeUtf8) ^. strict


-- | The target for windows.open().
data OpenTarget
  = OpenDOMSelf
  | OpenDOMBlank
  | OpenDOMParent
  | OpenDOMTop
  | OpenDOMUnfencedTop
  | OpenViewer
  deriving Show


instance ToHttpApiData OpenTarget where
  toUrlPiece = toUrlPiece . show


instance FromHttpApiData OpenTarget where
  parseUrlPiece "OpenDOMSelf"        = pure OpenDOMSelf
  parseUrlPiece "OpenDOMBlank"       = pure OpenDOMBlank
  parseUrlPiece "OpenDOMParent"      = pure OpenDOMParent
  parseUrlPiece "OpenDOMTop"         = pure OpenDOMTop
  parseUrlPiece "OpenDOMUnfencedTop" = pure OpenDOMUnfencedTop
  parseUrlPiece "OpenViewer"         = pure OpenViewer
  parseUrlPiece _                    = Left "unknown target for open()"


instance ToJSON OpenTarget where
  toJSON t = toJSON (show t)


data Manifest


instance Accept Manifest where
  contentType _ = "application/manifest+json"


instance MimeRender Manifest Value where
  mimeRender _ = Aeson.encode
