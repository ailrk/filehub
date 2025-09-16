{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module defines the Api type, thethe single Servant named routes for the
-- entire application.
--
-- == HTMX ==
-- Endpoints that return headers starting with `HX-` use HTMX’s HX-Trigger Response Headers feature.
-- These headers can instruct HTMX to perform actions such as redirecting the current page or firing
-- custom event handlers with a payload. For these endpoints to work correctly, HTMX must already be
-- loaded on the page.
--
-- == AuthProtect ==
-- Some endpoints are protected by the `AuthProtect` combinator. Most of the rules are implemented in
-- `Filehub.Server.Handler`.
module Filehub.Routes
  ( Api(..)
  , API
  , api
  )
  where

import Data.Data (Proxy(..))
import Data.Text (Text)
import Servant
    ( (:>),
      Get,
      PlainText,
      NamedRoutes,
      Get,
      (:-),
      QueryParam,
      Post,
      ReqBody,
      FormUrlEncoded,
      OctetStream,
      Delete,
      QueryFlag,
      Header,
      Headers,
      StreamGet,
      NoFraming,
      NoContent,
      CaptureAll,
      QueryParams,
      Capture
    )
import Lucid
import Lens.Micro.Platform ()
import Servant.Multipart (Mem, MultipartForm, MultipartData(..))
import Servant.HTML.Lucid (HTML)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData)
import Prelude hiding (readFile)
import Filehub.Types
    ( ClientPath(..)
    , Layout(..)
    , NewFile(..)
    , NewFolder(..)
    , SearchWord(..)
    , SortFileBy(..)
    , UpdatedFile(..)
    , Selected(..)
    , FilehubEvent(..)
    , SessionId
    , Resolution(..)
    , Manifest
    , LoginForm
    , OpenTarget
    , MoveFile
    , UIComponent
    )
import GHC.Generics (Generic)
import Filehub.Server.Handler (ConfirmReadOnly, ConfirmMobilOnly, ConfirmDesktopOnly, ConfirmLogin)
import Data.ByteString (ByteString)
import Conduit (ConduitT, ResourceT)
import Amazonka.Data (Value)
import Web.Cookie (SetCookie)
import Filehub.Locale (Locale)
import Target.Types (TargetId)


type instance AuthServerData (AuthProtect "session")      = SessionId
type instance AuthServerData (AuthProtect "readonly")     = ConfirmReadOnly
type instance AuthServerData (AuthProtect "desktop-only") = ConfirmDesktopOnly
type instance AuthServerData (AuthProtect "mobile-only")  = ConfirmMobilOnly
type instance AuthServerData (AuthProtect "login")        = ConfirmLogin


-- | Filehub custom headers are in format `X-Filehub-*`. They are usually used to report the server state
--   change to the frontend. E.g when /cancel is called, the server will clear the selection and copy state.
--   the new state like the total number of selected items is reported via `X-Filehub-Selected-Count`.


data Api mode = Api
  { init                  :: mode
                          :- "init"
                          :> AuthProtect "session"
                          :> ReqBody '[FormUrlEncoded] Resolution
                          :> Post '[HTML] (Html ())


  , home                  :: mode
                          :- AuthProtect "session"
                          :> AuthProtect "login"
                          :> Get '[HTML] (Html ())


  , refresh               :: mode
                          :- "refresh"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "component" UIComponent
                          :> Get '[HTML] (Html ())


  , loginPage             :: mode
                          :- "login"
                          :> AuthProtect "session"
                          :> Header "Cookie" Text
                          :> QueryParam "error" Text
                          :> Get '[HTML] (Html ())


  , loginToggleTheme      :: mode
                          :- "login" :> "theme" :> "toggle"
                          :> AuthProtect "session"
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))


  , loginChangeLocale     :: mode
                          :- "login" :> "locale" :> "change"
                          :> AuthProtect "session"
                          :> QueryParam "locale" Locale
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))


  , loginAuthSimple       :: mode
                          :- "login"
                          :> AuthProtect "session"
                          :> ReqBody '[FormUrlEncoded] LoginForm
                          :> Post '[HTML] (Headers '[ Header "Set-Cookie" SetCookie
                                                    , Header "HX-Redirect" Text
                                                    ] (Html ()))


  , loginAuthOIDCRedirect :: mode
                          :- "login" :> "oidc" :> "redirect"
                          :> AuthProtect "session"
                          :> Capture "provider_name" Text
                          :> Get '[HTML] NoContent


  , loginAuthOIDCCallback :: mode
                          :- "login" :> "oidc" :> "callback"
                          :> AuthProtect "session"
                          -- on success
                          :> QueryParam "code"  Text
                          :> QueryParam "state" Text
                          -- on error
                          :> QueryParam "error" Text
                          :> QueryParam "error_description" Text
                          -- shared
                          :> QueryParam "iss"   Text
                          :> QueryParam "state" Text
                          :> Get '[HTML]  NoContent

  , logout                :: mode
                          :- "logout"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> Post '[HTML] (Headers '[ Header "Set-Cookie" SetCookie
                                                    , Header "HX-Redirect" Text
                                                    ] NoContent)


  , cd                    :: mode
                          :- "cd"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "dir" ClientPath
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger-After-Swap" FilehubEvent ] (Html ()))


  , newFile               :: mode
                          :- "files" :> "new"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "readonly"
                          :> ReqBody '[FormUrlEncoded] NewFile
                          :> Post '[HTML] (Html ())


  , updateFile            :: mode
                          :- "files" :> "update"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "readonly"
                          :> ReqBody '[FormUrlEncoded] UpdatedFile
                          :> Post '[HTML] (Html ())


  , deleteFile            :: mode
                          :- "files" :> "delete"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "readonly"
                          :> QueryParams "file" ClientPath
                          :> QueryFlag "selected"
                          :> Delete '[HTML] (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))


  , copy                  :: mode
                          :- "files" :> "copy"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "readonly"
                          :> Get '[HTML] (Html ())


  , paste                 :: mode
                          :- "files" :> "paste"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "readonly"
                          :> Post '[HTML] (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))


  , move                  :: mode
                          :- "files" :> "move"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "readonly"
                          :> ReqBody '[FormUrlEncoded] MoveFile
                          :> Post '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))


  , newFolder             :: mode
                          :- "folders" :> "new"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "readonly"
                          :> ReqBody '[FormUrlEncoded] NewFolder
                          :> Post '[HTML] (Html ())


  , newFileModal          :: mode
                          :- "modal" :> "new-file"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "desktop-only"
                          :> AuthProtect "readonly"
                          :> Get '[HTML] (Html ())


  , newFolderModal        :: mode
                          :- "modal" :> "new-folder"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "desktop-only"
                          :> AuthProtect "readonly"
                          :> Get '[HTML] (Html ())


  , fileDetailModal       :: mode
                          :- "modal" :> "file" :> "detail"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "desktop-only"
                          :> QueryParam "file" ClientPath
                          :> Get '[HTML] (Html ())


  , editorModal           :: mode
                          :- "modal" :> "editor"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "file" ClientPath
                          :> Get '[HTML] (Html ())


  , search                :: mode
                          :- "search"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> ReqBody '[FormUrlEncoded] SearchWord
                          :> Post '[HTML] (Html ())


  , sortTable             :: mode
                          :- "table" :> "sort"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "by" SortFileBy
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))


  , selectLayout          :: mode
                          :- "layout"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "as" Layout
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))


  , selectRows            :: mode
                          :- "table" :> "select"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> ReqBody '[FormUrlEncoded] Selected
                          :> Post '[HTML] (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))


  , upload                :: mode
                          :- "upload"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "readonly"
                          :> MultipartForm Mem (MultipartData Mem)
                          :> Post '[HTML] (Html ())


  , download              :: mode
                          :- "download"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParams "file" ClientPath
                          :> StreamGet NoFraming OctetStream (Headers '[ Header "Content-Disposition" String ] (ConduitT () ByteString (ResourceT IO) ()))


  , cancel                :: mode
                          :- "cancel"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> Post '[HTML] (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))


  , contextMenu           :: mode
                          :- "contextmenu"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> AuthProtect "desktop-only"
                          :> QueryParams "file" ClientPath
                          :> Get '[HTML] (Html ())


  , initViewer            :: mode
                          :- "viewer"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "file" ClientPath
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent] NoContent)


  , open                  :: mode
                          :- "files"
                          :> "open"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "target" OpenTarget
                          :> QueryParam "file" ClientPath
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent] NoContent)


  , changeTarget          :: mode
                          :- "target" :> "change"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "target" TargetId
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger-After-Swap" FilehubEvent ] (Html ()))


  -- Servant api for      ces you to provide a content type at compile time, but we want to dynamically determine the content type instead.
  -- The current set       up will add two Content-Type headers to the response. One for octet-stream one for the actual content type.
  -- The type level       fix is too hacky, I decided to simply strip the unwanted header in a wai middleware.
  -- Check the dedup      HeadersKeepLast middleware, if there are duplicated headers, it will keep the last one. In this case we will
  -- discard the oct      et-stream and keep the content-type we set in the handler.
  , serve                 :: mode
                          :- "serve"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "file" ClientPath
                          :> StreamGet NoFraming OctetStream (Headers '[ Header "Content-Type" String
                                                                       , Header "Content-Disposition" String
                                                                       , Header "Cache-Control" String
                                                                       ] (ConduitT () ByteString (ResourceT IO) ()))


  -- Similar to serv      e but only serve image and pdf. Creates thumbnail version for requested image. This is useful for lazy loading image
  -- preview.
  , thumbnail             :: mode
                          :- "thumbnail"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> QueryParam "file" ClientPath
                          :> StreamGet NoFraming OctetStream (Headers '[ Header "Content-Type" String
                                                                       , Header "Content-Disposition" String
                                                                       , Header "Cache-Control" String
                                                                       ] (ConduitT () ByteString (ResourceT IO) ()))


  , themeCss              :: mode
                          :- "theme.css"
                          :> AuthProtect "session"
                          :> Get '[OctetStream] ByteString



  , toggleTheme           :: mode
                          :- "theme" :> "toggle"
                          :> AuthProtect "session"
                          :> AuthProtect "login"
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))

  , changeLocale          :: mode
                          :- "locale" :> "change"
                          :> AuthProtect "session"
                          :> QueryParam "locale" Locale
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))

  , manifest              :: mode :- "manifest.json" :> Get '[Manifest] Value


  , favicon               :: mode :- "favicon.ico" :> Get '[OctetStream] ByteString


  -- /static serves       static files, it faces the same problem /serve has.
  , static                :: mode :- "static" :> CaptureAll "file" FilePath :> Get '[OctetStream]  (Headers '[ Header "Content-Type" String
                                                                                                             , Header "Cache-Control" String
                                                                                                             ] ByteString)


  , offline               :: mode :- "offline" :> Get '[HTML] (Html ())


  , healthz               :: mode :- "healthz" :> Get '[PlainText] Text


#ifdef DEBUG
  , debug1                :: mode
                          :- "debug1"
                          :> AuthProtect "session"
                          :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent] NoContent)
#endif
  }
  deriving Generic


type API = NamedRoutes Api


api :: Proxy API
api = Proxy @API
