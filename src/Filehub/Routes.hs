{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

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
    )
import Lucid
import Lens.Micro.Platform ()
import Data.ByteString.Lazy qualified as LBS
import Servant.Multipart (Mem, MultipartForm, MultipartData(..))
import Servant.HTML.Lucid (HTML)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData)
import Prelude hiding (readFile)
import Filehub.Types
    ( ClientPath(..),
      NewFile(..),
      NewFolder(..),
      SearchWord(..),
      SortFileBy(..),
      UpdatedFile(..),
      TargetId,
      Selected(..),
      FilehubEvent(..),
      SessionId,
      Resolution(..), Manifest
    )
import GHC.Generics (Generic)
import Filehub.Server.Context.Resolution (ConfirmMobilOnly, ConfirmDesktopOnly)
import Filehub.Server.Context.ReadOnly (ConfirmReadOnly)
import Filehub.Layout (Layout)
import Data.ByteString (ByteString)
import Conduit (ConduitT, ResourceT)
import Amazonka.Data (Value)


type instance AuthServerData (AuthProtect "session") = SessionId
type instance AuthServerData (AuthProtect "readonly") = ConfirmReadOnly
type instance AuthServerData (AuthProtect "desktop-only") = ConfirmDesktopOnly
type instance AuthServerData (AuthProtect "mobile-only") = ConfirmMobilOnly

-- | Filehub custom headers are in format `X-Filehub-*`. They are usually used to report the server state
--   change to the frontend. E.g when /cancel is called, the server will clear the selection and copy state.
--   the new state like the total number of selected items is reported via `X-Filehub-Selected-Count`.


data Api mode = Api
  { init            :: mode :- "init"
                    :> AuthProtect "session"
                    :> ReqBody '[FormUrlEncoded] Resolution
                    :> Post '[HTML] (Html ())


  , index           :: mode :- AuthProtect "session" :> Get '[HTML] (Html ())


  , cd              :: mode :- "cd"
                    :> AuthProtect "session"
                    :> QueryParam "dir" ClientPath
                    :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))


  , newFile         :: mode :- "files"
                    :> "new"
                    :> AuthProtect "session"
                    :> AuthProtect "readonly"
                    :> ReqBody '[FormUrlEncoded] NewFile
                    :> Post '[HTML] (Html ())


  , updateFile      :: mode :- "files"
                    :> "update"
                    :> AuthProtect "session"
                    :> AuthProtect "readonly"
                    :> ReqBody '[FormUrlEncoded] UpdatedFile
                    :> Post '[HTML] (Html ())


  , deleteFile      :: mode :- "files"
                    :> "delete"
                    :> AuthProtect "session"
                    :> AuthProtect "readonly"
                    :> QueryParam "file" ClientPath
                    :> QueryFlag "selected"
                    :> Delete '[HTML] (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))


  , copy            :: mode :- "files"
                    :> "copy"
                    :> AuthProtect "session"
                    :> AuthProtect "readonly"
                    :> Get '[HTML] (Html ())


  , paste           :: mode :- "files"
                    :> "paste"
                    :> AuthProtect "session"
                    :> AuthProtect "readonly"
                    :> Post '[HTML] (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))


  , newFolder       :: mode :- "folders"
                    :> "new"
                    :> AuthProtect "session"
                    :> AuthProtect "readonly"
                    :> ReqBody '[FormUrlEncoded] NewFolder
                    :> Post '[HTML] (Html ())


  , newFileModal    :: mode :- "modal"
                    :> "new-file"
                    :> AuthProtect "session"
                    :> AuthProtect "desktop-only"
                    :> AuthProtect "readonly"
                    :> Get '[HTML] (Html ())


  , newFolderModal  :: mode :- "modal"
                    :> "new-folder"
                    :> AuthProtect "session"
                    :> AuthProtect "desktop-only"
                    :> AuthProtect "readonly"
                    :> Get '[HTML] (Html ())


  , fileDetailModal :: mode :- "modal"
                    :> "file"
                    :> "detail"
                    :> AuthProtect "session"
                    :> AuthProtect "desktop-only"
                    :> QueryParam "file" ClientPath
                    :> Get '[HTML] (Html ())


  , editorModal     :: mode :- "modal"
                    :> "editor"
                    :> AuthProtect "session"
                    :> QueryParam "file" ClientPath
                    :> Get '[HTML] (Html ())


  , search          :: mode :- "search"
                    :> AuthProtect "session"
                    :> ReqBody '[FormUrlEncoded] SearchWord
                    :> Post '[HTML] (Html ())


  , sortTable       :: mode :- "table"
                    :> "sort"
                    :> AuthProtect "session"
                    :> QueryParam "by" SortFileBy
                    :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))

  , selectLayout    :: mode :- "layout"
                    :> AuthProtect "session"
                    :> QueryParam "as" Layout
                    :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))

  , selectRows      :: mode :- "table"
                    :> "select"
                    :> AuthProtect "session"
                    :> ReqBody '[FormUrlEncoded] Selected
                    :> Post '[HTML] (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))


  , upload          :: mode :- "upload"
                    :> AuthProtect "session"
                    :> AuthProtect "readonly"
                    :> MultipartForm Mem (MultipartData Mem)
                    :> Post '[HTML] (Html ())


  , download        :: mode :- "download"
                    :> AuthProtect "session"
                    :> QueryParam "file" ClientPath
                    :> StreamGet NoFraming OctetStream (Headers '[ Header "Content-Disposition" String ] (ConduitT () ByteString (ResourceT IO) ()))


  , cancel          :: mode :- "cancel"
                    :> AuthProtect "session"
                    :> Post '[HTML] (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))


  , contextMenu     :: mode :- "contextmenu"
                    :> AuthProtect "session"
                    :> AuthProtect "desktop-only"
                    :> QueryParam "file" ClientPath
                    :> Get '[HTML] (Html ())


  , initViewer      :: mode :- "viewer"
                    :> AuthProtect "session"
                    :> QueryParam "file" ClientPath
                    :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent] NoContent)


  , changeTarget    :: mode :- "target"
                    :> "change"
                    :> AuthProtect "session"
                    :> QueryParam "target" TargetId
                    :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))


  -- Servant api forces you to provide a content type at compile time, but we want to dynamically determine the content type instead.
  -- The current set up will add two Content-Type headers to the response. One for octet-stream one for the actual content type.
  -- The type level fix is too hacky, I decided to simply strip the unwanted header in a wai middleware.
  -- Check the dedupHeadersKeepLast middleware, if there are duplicated headers, it will keep the last one. In this case we will
  -- discard the octet-stream and keep the content-type we set in the handler.
  , serve           :: mode :- "serve"
                    :> AuthProtect "session"
                    :> QueryParam "file" ClientPath
                    :> StreamGet NoFraming OctetStream (Headers '[ Header "Content-Type" String
                                                                 , Header "Content-Disposition" String
                                                                 ] (ConduitT () ByteString (ResourceT IO) ()))


  , themeCss        :: mode :- "theme.css" :> Get '[OctetStream] LBS.ByteString


  , manifest        :: mode :- "manifest.json" :> Get '[Manifest] Value


  , favicon         :: mode :- "favicon.ico" :> Get '[OctetStream] LBS.ByteString


  -- /static serves static files, it faces the same problem /serve has.
  , static          :: mode :- "static" :> CaptureAll "file" FilePath :> Get '[OctetStream]  (Headers '[ Header "Content-Type" String
                                                                                                       ] LBS.ByteString)


  , offline         :: mode :- "offline" :> Get '[HTML] (Html ())


  , healthz         :: mode :- "healthz" :> Get '[PlainText] Text
  }
  deriving Generic


type API = NamedRoutes Api


api :: Proxy API
api = Proxy @API
