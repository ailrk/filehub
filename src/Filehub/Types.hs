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
  , RenameFile(..)
  , Theme(..)
  , FilehubEvent(..)
  , OpenTarget (..)
  , UIComponent(..)
  , Resource(..)
  , Manifest
  )
  where

import Data.Aeson (ToJSON (..), (.=), Value, Key)
import Data.Aeson qualified as Aeson
import Data.ClientPath (ClientPath(..), RawClientPath(..))
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Filehub.Display (Display(..), Resolution(..))
import Filehub.Env (Env(..))
import Filehub.Session.Types (SessionId(..), Session(..), TargetSessionData(..))
import Filehub.Sort (SortFileBy(..))
import Filehub.Theme (Theme(..))
import GHC.Generics (Generic)
import GHC.IsList (fromList)
import Servant ( ToHttpApiData(..), FromHttpApiData(..), Accept (..), MimeRender )
import Servant.API (MimeRender(..))
import Web.FormUrlEncoded (FromForm (..), parseUnique, ToForm (..), parseAll)
import Lucid.Htmx (IsHtmxCustomEvent (..), asCamelCase)
import Data.Text qualified as T
import Data.Aeson.Key qualified as Aeson.Key


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


newtype SearchWord = SearchWord Text deriving (Show, Eq, Generic)
instance FromForm SearchWord where fromForm f = SearchWord <$> parseUnique "search" f


newtype NewFile = NewFile Text deriving (Show, Eq, Generic)
instance FromForm NewFile where fromForm f = NewFile <$> parseUnique "new-file" f


newtype NewFolder = NewFolder Text deriving (Show, Eq, Generic)
instance FromForm NewFolder where fromForm f = NewFolder <$> parseUnique "new-folder" f


data MoveFile = MoveFile [ClientPath] ClientPath deriving (Show, Eq)
instance FromForm MoveFile where fromForm f = MoveFile <$> parseAll "src" f <*> parseUnique "tgt" f


data RenameFile = RenameFile ClientPath String deriving (Show, Eq)
instance FromForm RenameFile where fromForm f = RenameFile <$> parseUnique "old" f <*> parseUnique "new" f


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
  toUrlPiece v = (TL.toStrict $ TL.decodeUtf8 $ Aeson.encode $ v)


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
  | SidebarToggled
  | FileMoved
  | FileRenamed
  | Canceled -- Action canceled
  | Opened OpenTarget ClientPath -- load a resource into tab/window/iframe. Hook  for window.open
  deriving (Show)


instance IsHtmxCustomEvent FilehubEvent where
  htmxCustomEventNameToText (ViewerInited {}) = "ViewerInited"
  htmxCustomEventNameToText (Opened {}) = "Opened"
  htmxCustomEventNameToText e = T.pack (show e)


eventNameAsKey :: FilehubEvent -> Key
eventNameAsKey = Aeson.Key.fromText . asCamelCase . htmxCustomEventNameToText


instance ToJSON FilehubEvent where
  toJSON e@(ViewerInited res index) =
    Aeson.object
      [ eventNameAsKey e .= Aeson.object
          [ "resources" .= toJSON res
          , "index"     .= toJSON index
          ]
      ]
  toJSON e@(Opened target path) =
    Aeson.object
      [ eventNameAsKey e .= Aeson.object
          [ "path" .= toJSON path
          , "tgt"  .= toJSON target
          ]
      ]
  toJSON e = Aeson.object [ eventNameAsKey e .= Aeson.object [] ]


instance ToHttpApiData FilehubEvent where
  toUrlPiece v = (TL.toStrict $ TL.decodeUtf8 $ Aeson.encode $ v)


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
