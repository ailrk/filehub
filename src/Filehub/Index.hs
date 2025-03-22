{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Filehub.Index where

import Lucid
import Lens.Micro
import Lens.Micro.Platform ()
import Effectful (Eff, (:>), IOE)
import Effectful.Error.Dynamic (runErrorNoCallStack, throwError, Error)
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import System.FilePath ((</>), takeFileName)
import GHC.Generics (Generic)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as LBS
import Data.String (IsString(..))
import Data.Maybe (fromMaybe)
import Servant.Multipart (Mem, MultipartForm, MultipartData(..))
import Servant.Server.Generic (AsServerT)
import Servant (Get, (:-), QueryParam, Post, Put, ReqBody, FormUrlEncoded, OctetStream, addHeader, noHeader, Delete, ServerError (errBody))
import Servant qualified as S
import Servant.HTML.Lucid (HTML)
import Servant.Server (err400, err500)
import Text.Printf (printf)
import Prelude hiding (readFile)
import Filehub (Filehub)
import Filehub.Template qualified as Template
import Filehub.Env qualified as Env
import Filehub.Domain.Types (ClientPath(..), NewFile (..), NewFolder(..), SearchWord(..), SortFileBy(..), UpdatedFile (..), Theme (..), FilehubError(..))
import Filehub.Domain.File (sortFiles)
import Filehub.Domain qualified as Domain
import Filehub.Cookie qualified as Cookies
import Filehub.Cookie (Cookies' (..), SetCookie)
import Filehub.Types (Session(..), SessionId(..))
import Filehub.SessionPool qualified as SessionPool
import Effectful.Reader.Dynamic (Reader)


data Api mode = Api
  { index           :: mode :- S.Header "Cookie" Cookies'
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , cd              :: mode :- "cd"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "dir" ClientPath
                    S.:> Get '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                 , S.Header "HX-Trigger" FilehubError
                                                 ]
                                                 (Html ()))


  , newFile         :: mode :- "files"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "new"
                    S.:> ReqBody '[FormUrlEncoded] NewFile
                    S.:> Post '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                  , S.Header "HX-Trigger" FilehubError
                                                  ]
                                                 (Html ()))


  , updateFile      :: mode :- "files"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "update"
                    S.:> ReqBody '[FormUrlEncoded] UpdatedFile
                    S.:> Put '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , deleteFile      :: mode :- "files"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "delete"
                    S.:> QueryParam "file" ClientPath
                    S.:> Delete '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , newFolder       :: mode :- "folders"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "new"
                    S.:> ReqBody '[FormUrlEncoded] NewFolder
                    S.:> Post '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                  , S.Header "HX-Trigger" FilehubError
                                                  ] (Html ()))


  , newFileModal    :: mode :- "modal"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "new-file"
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , newFolderModal  :: mode :- "modal"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "new-folder"
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , fileDetailModal :: mode :- "modal"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "file"
                    S.:> "detail"
                    S.:> QueryParam "file" ClientPath
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , uploadModal     :: mode :- "modal"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "upload"
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , editorModal     :: mode :- "modal"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "editor"
                    S.:> QueryParam "file" ClientPath
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , search          :: mode :- "search"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> ReqBody '[FormUrlEncoded] SearchWord
                    S.:> Post '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , sortTable       :: mode :- "table"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "sort"
                    S.:> QueryParam "by" SortFileBy
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


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


  , contextMenu     :: mode :- "contextmenu"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "file" ClientPath
                    S.:> Get '[HTML] (S.Headers '[S.Header "Set-Cookie" SetCookie] (Html ()))


  , initViewer      :: mode :- "viewer"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> QueryParam "file" ClientPath
                    S.:> Get '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                 , S.Header "HX-Trigger" Domain.Viewer
                                                 ] (Html ()))


  , themeCss        :: mode :- "theme.css"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> Get '[OctetStream] (S.Headers '[S.Header "Set-Cookie" SetCookie] LBS.ByteString)
  }
  deriving (Generic)


-- | Get sessionId from cookie if it exists. If it doesn't, create a new session and add
--   `SetCookie` header to set the sessionId.
--   Note: The order of headers in the Headers HLIST matters.
withSession :: ( Reader Env.Env :> es, IOE :> es, S.AddHeader [S.Optional, S.Strict] h SetCookie a b) => Maybe Cookies' -> (SessionId -> Eff es a) -> Eff es b
withSession mCookie cont =
  case mCookie >>= Cookies.getSessionId of
    Just sessionId -> noHeader <$> cont sessionId
    Nothing -> do
      session <- SessionPool.newSession
      let setCookie = Cookies.setSessionId session
      addHeader setCookie <$> cont session.sessionId


withClientPath :: (Error ServerError :> es) => Maybe ClientPath -> (ClientPath -> Eff es a) -> Eff es a
withClientPath mClientPath cont =
  case mClientPath of
    Just clientPath -> cont clientPath
    Nothing -> throwError err400


server :: Api (AsServerT Filehub)
server = Api
  { index = \mCookie -> withSession mCookie index


  , cd = \mCookie mClientPath -> do
      withClientPath mClientPath $ \clientPath ->
        withSession mCookie $ \sessionId -> do
        root <- Env.getRoot
        r <- Domain.changeDir sessionId (Domain.fromClientPath root clientPath) & runErrorNoCallStack
        let header = either addHeader (const noHeader) r
        header <$> view sessionId


  , newFile = \mCookie (NewFile path) -> do
      withSession mCookie $ \sessionId -> do
        r <- Domain.newFile sessionId (Text.unpack path) & runErrorNoCallStack
        let header = either addHeader (const noHeader) r
        header <$> view sessionId


  , updateFile = \mCookie (UpdatedFile clientPath content) -> do
      withSession mCookie $ \ sessionId -> do
        let path = clientPath.unClientPath
        Domain.writeFile sessionId path (Text.encodeUtf8 content ^. lazy)
        view sessionId


  , deleteFile = \mCookie mClientPath ->
      withClientPath mClientPath $ \clientPath ->
        withSession mCookie $ \sessionId -> do
        p <- Domain.fromClientPath <$> Env.getRoot <*> pure clientPath
        Domain.deleteFile sessionId p
        view sessionId


  , newFolder = \mCookie (NewFolder path) ->
      withSession mCookie $ \sessionId -> do
        r <- Domain.newFolder sessionId (Text.unpack path) & runErrorNoCallStack
        let header = either addHeader (const noHeader) r
        header <$> view sessionId


  , newFileModal = \mCookie -> withSession mCookie $ const (pure Template.newFileModal)


  , newFolderModal = \mCookie -> withSession mCookie $ const (pure Template.newFolderModal)


  , fileDetailModal = \mCookie mClientPath ->
      withClientPath mClientPath $ \clientPath ->
        withSession mCookie $ \_ -> do
        p <- Domain.fromClientPath <$> Env.getRoot <*> pure clientPath
        file <- Domain.getFile p & withServerError
        pure (Template.fileDetailModal file)


  , uploadModal = \mCookie -> withSession mCookie $ const (pure Template.uploadModal)


  , editorModal = \mCookie mClientPath ->
      withClientPath mClientPath $ \clientPath ->
        withSession mCookie $ \_ -> do
        p <- Domain.fromClientPath <$> Env.getRoot <*> pure clientPath
        content <- Domain.getFile p & withServerError >>= Domain.readFileContent
        let filename = takeFileName p
        pure $ Template.editorModal filename content


  , search = \mCookie searchWord ->
      withSession mCookie $ \sessionId ->
        Template.search searchWord
        <$> Env.getRoot
        <*> withServerError (Domain.lsCurrentDir sessionId)


  , sortTable = \mCookie order -> do
      withSession mCookie $ \sessionId -> do
        Env.setSortFileBy sessionId (fromMaybe ByName order)
        view sessionId


  , upload = \mCookie multipart ->
      withSession mCookie $ \sessionId -> do
        Domain.upload sessionId multipart & withServerError
        index sessionId


  , download = \mCookie mClientPath ->
      withClientPath mClientPath $ \clientPath@(ClientPath path)->
        withSession mCookie $ \_ -> do
        bs <- Domain.download clientPath & withServerError
        pure $ addHeader (printf "attachement; filename=%s.zip" (takeFileName path)) bs


  , contextMenu = \mCookie mClientPath ->
      withClientPath mClientPath $ \clientPath ->
        withSession mCookie $ \_ -> do
        root <- Env.getRoot
        let filePath = Domain.fromClientPath root clientPath
        file <- Domain.getFile filePath & withServerError
        pure $ Template.contextMenu root file


  , initViewer = \mCookie mClientPath ->
      withClientPath mClientPath $ \clientPath ->
        withSession mCookie $ \sessionId -> do
        root <- Env.getRoot
        payload <- Domain.initViewer sessionId root clientPath & withServerError
        pure $ addHeader payload mempty


  , themeCss = \mCookie ->
      withSession mCookie $ \_ -> do
        theme <- Env.getTheme
        dir <- Env.getDataDir
        readFile $
          case theme of
            Dark1 -> dir </> "dark1.css"
            Dark2 -> dir </> "dark2.css"
            Dark3 -> dir </> "dark3.css"
            Light1 -> dir </> "light1.css"
            Light2 -> dir </> "light2.css"
            Light3 -> dir </> "light3.css"
  }


withServerError :: (Error ServerError :> es) => Eff (Error FilehubError : es) b -> Eff es b
withServerError action = do
  runErrorNoCallStack action >>= \case
    Left err -> throwError err500 { errBody = fromString $ show err }
    Right res -> pure res


index :: SessionId -> Filehub (Html ())
index sessionId = Template.index <$> view sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  order <- Env.getSortFileBy sessionId
  let table = Template.table
          <$> Env.getRoot
          <*> (sortFiles order <$> withServerError (Domain.lsCurrentDir sessionId))
  Template.view <$> table <*> pathBreadcrumb sessionId


pathBreadcrumb :: SessionId -> Filehub (Html ())
pathBreadcrumb sessionId = Template.pathBreadcrumb <$> Env.getCurrentDir sessionId <*> Env.getRoot
