{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

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
      Delete, QueryFlag )
import Lucid
import Lens.Micro.Platform ()
import Data.ByteString.Lazy qualified as LBS
import Servant.Multipart (Mem, MultipartForm, MultipartData(..))
import Servant qualified as S
import Servant.HTML.Lucid (HTML)
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
      FilehubEvent(..)
    )
import Filehub.Cookie (Cookies' (..), SetCookie)
import GHC.Generics (Generic)


data Api mode = Api
  { index           :: mode :- S.Header "Cookie" Cookies'
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , cd              :: mode :- "cd"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "dir" ClientPath
                    S.:> Get '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                 , S.Header "HX-Trigger" FilehubError
                                                 , S.Header "HX-Trigger" FilehubEvent
                                                 ]
                                                 (Html ()))


  , newFile         :: mode :- "files"
                    S.:> "new"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> ReqBody '[FormUrlEncoded] NewFile
                    S.:> Post '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                  , S.Header "HX-Trigger" FilehubError
                                                  ]
                                                 (Html ()))


  , updateFile      :: mode :- "files"
                    S.:> "update"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> ReqBody '[FormUrlEncoded] UpdatedFile
                    S.:> Put '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , deleteFile      :: mode :- "files"
                    S.:> "delete"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "file" ClientPath
                    S.:> QueryFlag "selected"
                    S.:> Delete '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , copy            :: mode :- "files"
                    S.:> "copy"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> Get '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie] (Html ()))


  , paste           :: mode :- "files"
                    S.:> "paste"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> Get '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie] (Html ()))


  , newFolder       :: mode :- "folders"
                    S.:> "new"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> ReqBody '[FormUrlEncoded] NewFolder
                    S.:> Post '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                  , S.Header "HX-Trigger" FilehubError
                                                  ] (Html ()))


  , newFileModal    :: mode :- "modal"
                    S.:> "new-file"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , newFolderModal  :: mode :- "modal"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "new-folder"
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , fileDetailModal :: mode :- "modal"
                    S.:> "file"
                    S.:> "detail"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "file" ClientPath
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , uploadModal     :: mode :- "modal"
                    S.:> "upload"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , editorModal     :: mode :- "modal"
                    S.:> "editor"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "file" ClientPath
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , search          :: mode :- "search"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> ReqBody '[FormUrlEncoded] SearchWord
                    S.:> Post '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , sortTable       :: mode :- "table"
                    S.:> "sort"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "by" SortFileBy
                    S.:> Get '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                 , S.Header "HX-Trigger" FilehubEvent
                                                 ] (Html ()))


  , selectRows      :: mode :- "table"
                    S.:> "select"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> ReqBody '[FormUrlEncoded] Selected
                    S.:> Post '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie ] (Html ()))


  , upload          :: mode :- "upload"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> MultipartForm Mem (MultipartData Mem)
                    S.:> Post '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , download        :: mode :- "download"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "file" ClientPath
                    S.:> Get '[OctetStream] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                        , S.Header "Content-Disposition" String
                                                        ] LBS.ByteString)


  , cancel          :: mode :- "cancel"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , contextMenu     :: mode :- "contextmenu"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "file" ClientPath
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , initViewer      :: mode :- "viewer"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "file" ClientPath
                    S.:> Get '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                 , S.Header "HX-Trigger" FilehubEvent
                                                 ] (Html ()))

  , changeTarget    :: mode :- "target"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "change"
                    S.:> QueryParam "target" TargetId
                    S.:> Get '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                 , S.Header "HX-Trigger" FilehubEvent
                                                 ] (Html ()))


  , themeCss        :: mode :- "theme.css"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> Get '[OctetStream] (S.Headers '[S.Header "Set-Cookie" SetCookie] LBS.ByteString)


  , healthz :: mode :- "healthz" :> Get '[PlainText] Text
  }
  deriving Generic


type API = NamedRoutes Api
      :<|> "static" :> Raw -- static files for the app
      :<|> Raw -- direct access of the underlying directory


api :: Proxy API
api = Proxy @API
