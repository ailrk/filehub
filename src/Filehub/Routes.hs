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
      (:-),
      Raw,
      (:<|>)(..),
      Get,
      (:-),
      QueryParam,
      Post,
      Put,
      ReqBody,
      FormUrlEncoded,
      OctetStream,
      Delete,
      QueryFlag,
      Header,
      Headers
    )
import Lucid
import Lens.Micro.Platform ()
import Data.ByteString.Lazy qualified as LBS
import Servant.Multipart (Mem, MultipartForm, MultipartData(..))
import Servant.HTML.Lucid (HTML)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData)
import Prelude hiding (readFile)
import Filehub.Error (FilehubError(..))
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
      SessionId
    )
import GHC.Generics (Generic)


type instance AuthServerData (AuthProtect "session") = SessionId


data Api mode = Api
  { index           :: mode :- AuthProtect "session" :> Get '[HTML] (Html ())


  , cd              :: mode :- "cd"
                    :> AuthProtect "session"
                    :> QueryParam "dir" ClientPath
                    :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubError
                                             , Header "HX-Trigger" FilehubEvent
                                             ] (Html ()))


  , newFile         :: mode :- "files"
                    :> "new"
                    :> AuthProtect "session"
                    :> ReqBody '[FormUrlEncoded] NewFile
                    :> Post '[HTML] (Headers '[ Header "HX-Trigger" FilehubError ] (Html ()))


  , updateFile      :: mode :- "files"
                    :> "update"
                    :> AuthProtect "session"
                    :> ReqBody '[FormUrlEncoded] UpdatedFile
                    :> Put '[HTML] (Html ())


  , deleteFile      :: mode :- "files"
                    :> "delete"
                    :> AuthProtect "session"
                    :> QueryParam "file" ClientPath
                    :> QueryFlag "selected"
                    :> Delete '[HTML] (Html ())


  , copy            :: mode :- "files"
                    :> "copy"
                    :> AuthProtect "session"
                    :> Get '[HTML] (Html ())


  , paste           :: mode :- "files"
                    :> "paste"
                    :> AuthProtect "session"
                    :> Get '[HTML] (Html ())


  , newFolder       :: mode :- "folders"
                    :> "new"
                    :> AuthProtect "session"
                    :> ReqBody '[FormUrlEncoded] NewFolder
                    :> Post '[HTML] (Headers '[ Header "HX-Trigger" FilehubError ] (Html ()))


  , newFileModal    :: mode :- "modal"
                    :> "new-file"
                    :> AuthProtect "session"
                    :> Get '[HTML] (Html ())


  , newFolderModal  :: mode :- "modal"
                    :> "new-folder"
                    :> AuthProtect "session"
                    :> Get '[HTML] (Html ())


  , fileDetailModal :: mode :- "modal"
                    :> "file"
                    :> "detail"
                    :> AuthProtect "session"
                    :> QueryParam "file" ClientPath
                    :> Get '[HTML] (Html ())


  , uploadModal     :: mode :- "modal"
                    :> "upload"
                    :> AuthProtect "session"
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


  , selectRows      :: mode :- "table"
                    :> "select"
                    :> AuthProtect "session"
                    :> ReqBody '[FormUrlEncoded] Selected
                    :> Post '[HTML] (Html ())


  , upload          :: mode :- "upload"
                    :> AuthProtect "session"
                    :> MultipartForm Mem (MultipartData Mem)
                    :> Post '[HTML] (Html ())


  , download        :: mode :- "download"
                    :> AuthProtect "session"
                    :> QueryParam "file" ClientPath
                    :> Get '[OctetStream] (Headers '[ Header "Content-Disposition" String ] LBS.ByteString)


  , cancel          :: mode :- "cancel"
                    :> AuthProtect "session"
                    :> Get '[HTML] (Html ())


  , contextMenu     :: mode :- "contextmenu"
                    :> AuthProtect "session"
                    :> QueryParam "file" ClientPath
                    :> Get '[HTML] (Html ())


  , initViewer      :: mode :- "viewer"
                    :> AuthProtect "session"
                    :> QueryParam "file" ClientPath
                    :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))

  , changeTarget    :: mode :- "target"
                    :> "change"
                    :> AuthProtect "session"
                    :> QueryParam "target" TargetId
                    :> Get '[HTML] (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))


  , themeCss        :: mode :- "theme.css" :> Get '[OctetStream] LBS.ByteString


  , healthz :: mode :- "healthz" :> Get '[PlainText] Text
  }
  deriving Generic


type API = NamedRoutes Api
      :<|> "static" :> Raw -- static files for the app
      :<|> Raw -- direct access of the underlying directory


api :: Proxy API
api = Proxy @API
