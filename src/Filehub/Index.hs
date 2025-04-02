{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Filehub.Index where

import Lucid
import Lens.Micro
import Lens.Micro.Platform ()
import Effectful (Eff, (:>), IOE, liftIO)
import Effectful.Reader.Dynamic (Reader)
import Effectful.Error.Dynamic (runErrorNoCallStack, throwError, Error)
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import System.FilePath ((</>), takeFileName)
import GHC.Generics (Generic)
import Control.Monad (when)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as LBS
import Data.String (IsString(..))
import Data.Maybe (fromMaybe)
import Data.Time.Clock qualified as Time
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
import Filehub.Domain qualified as Domain
import Filehub.Domain.Viewer (Viewer(..))
import Filehub.Domain.Viewer qualified as Viewer
import Filehub.Cookie qualified as Cookies
import Filehub.Cookie (Cookies' (..), SetCookie)
import Filehub.Types (Session(..), SessionId(..), TargetId)
import Filehub.Storage (Storage)
import Filehub.Storage qualified as Storage
import Filehub.Env.SessionPool qualified as SessionPool
import Filehub.Env.Target qualified as Target


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
                                                 , S.Header "HX-Trigger" Viewer
                                                 ] (Html ()))


  , changeTarget    :: mode :- "target"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> "change"
                    S.:> QueryParam "target" TargetId
                    S.:> Get '[HTML] (S.Headers '[ S.Header "Set-Cookie" SetCookie
                                                 ] (Html ()))

  , themeCss        :: mode :- "theme.css"
                    S.:> S.Header "Cookie" Cookies'
                    S.:> Get '[OctetStream] (S.Headers '[S.Header "Set-Cookie" SetCookie] LBS.ByteString)
  }
  deriving (Generic)


-- | Get sessionId from cookie if it exists. If it doesn't, create a new session and add
--   `SetCookie` header to set the sessionId.
--   We extend it's duration when the sessionId exists and it will expiry in 30 seconds
--   Note: The order of headers in the Headers HLIST matters.
withSession
  :: (Reader Env.Env :> es, IOE :> es, S.AddHeader [S.Optional, S.Strict] h SetCookie a b)
  => Maybe Cookies' -> (SessionId -> Eff es a) -> Eff es b
withSession mCookie cont =
  case mCookie >>= Cookies.getSessionId' of
    Just sessionId -> do
      SessionPool.getSession sessionId >>= \case
        Just session -> do
          now <- liftIO Time.getCurrentTime
          let diff = Time.nominalDiffTimeToSeconds $ session.expireDate `Time.diffUTCTime` now
          when (diff >= 0 && diff < 30) $ do
            SessionPool.extendSession sessionId
          noHeader <$> cont sessionId
        Nothing -> contWithNewSession
    Nothing -> contWithNewSession
  where
    contWithNewSession = do
      session <- SessionPool.newSession
      let setCookie = Cookies.setSessionId session
      addHeader setCookie <$> cont session.sessionId


withQueryParam :: (Error ServerError :> es) => Maybe a -> Eff es a
withQueryParam m =
  case m of
    Just a -> pure a
    Nothing -> throwError err400


withServerError :: (Error ServerError :> es) => Eff (Error FilehubError : es) b -> Eff es b
withServerError action = do
  runErrorNoCallStack action >>=
    either (\err -> throwError err500 { errBody = fromString $ show err }) pure


server :: Api (AsServerT Filehub)
server = Api
  { index = \mCookie -> withSession mCookie (fmap Template.withDefault . index)


  , cd = \mCookie mClientPath -> do
      withSession mCookie $ \sessionId -> do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId & withServerError
        r <- runStorage $ Storage.changeDir sessionId (Domain.fromClientPath root clientPath) & runErrorNoCallStack
        let header = either addHeader (const noHeader) r
        header <$> view sessionId


  , newFile = \mCookie (NewFile path) -> do
      withSession mCookie $ \sessionId -> do
        r <- runStorage $ Storage.newFile sessionId (Text.unpack path) & runErrorNoCallStack
        let header = either addHeader (const noHeader) r
        header <$> view sessionId


  , updateFile = \mCookie (UpdatedFile clientPath content) -> do
      withSession mCookie $ \ sessionId -> do
        let path = clientPath.unClientPath
        _ <- runStorage $ Storage.writeFile sessionId path (Text.encodeUtf8 content ^. lazy)
        view sessionId


  , deleteFile = \mCookie mClientPath ->
      withSession mCookie $ \sessionId -> do
        withServerError do
          clientPath <- withQueryParam mClientPath
          root <- Env.getRoot sessionId
          let p = Domain.fromClientPath root clientPath
          runStorage $ Storage.deleteFile sessionId p
        view sessionId


  , newFolder = \mCookie (NewFolder path) ->
      withSession mCookie $ \sessionId -> do
        r <- runStorage do
          Storage.newFolder sessionId (Text.unpack path) & runErrorNoCallStack
        let header = either addHeader (const noHeader) r
        header <$> view sessionId


  , newFileModal = \mCookie -> withSession mCookie $ const (pure Template.newFileModal)


  , newFolderModal = \mCookie -> withSession mCookie $ const (pure Template.newFolderModal)


  , fileDetailModal = \mCookie mClientPath ->
      withSession mCookie $ \sessionId ->
        withServerError do
          clientPath <- withQueryParam mClientPath
          root <- Env.getRoot sessionId
          file <- runStorage $ Storage.getFile (Domain.fromClientPath root clientPath)
          pure (Template.fileDetailModal file)


  , uploadModal = \mCookie -> withSession mCookie $ const (pure Template.uploadModal)


  , editorModal = \mCookie mClientPath ->
      withSession mCookie $ \sessionId -> do
        withServerError do
          clientPath <- withQueryParam mClientPath
          root <- Env.getRoot sessionId
          let p = Domain.fromClientPath root clientPath
          content <- runStorage do
            f <- Storage.getFile p
            Storage.readFileContent f
          let filename = takeFileName p
          pure $ Template.editorModal filename content


  , search = \mCookie searchWord ->
      withSession mCookie $ \sessionId ->
        withServerError . runStorage $ do
          Template.search searchWord
          <$> Env.getRoot sessionId
          <*> Storage.lsCurrentDir sessionId


  , sortTable = \mCookie order -> do
      withSession mCookie $ \sessionId -> do
        Env.setSortFileBy sessionId (fromMaybe ByName order)
        view sessionId


  , upload = \mCookie multipart ->
      withSession mCookie $ \sessionId -> do
        runStorage $ Storage.upload sessionId multipart
        index sessionId


  , download = \mCookie mClientPath ->
      withSession mCookie $ \sessionId -> do
        clientPath@(ClientPath path) <- withQueryParam mClientPath
        bs <- runStorage $ Storage.download sessionId clientPath
        pure $ addHeader (printf "attachement; filename=%s" (takeFileName path)) bs


  , contextMenu = \mCookie mClientPath ->
      withSession mCookie $ \sessionId ->
        withServerError do
          clientPath <- withQueryParam mClientPath
          root <- Env.getRoot sessionId
          let filePath = Domain.fromClientPath root clientPath
          file <- runStorage $ Storage.getFile filePath
          pure $ Template.contextMenu root file


  , initViewer = \mCookie mClientPath ->
      withSession mCookie $ \sessionId ->
        withServerError do
          clientPath <- withQueryParam mClientPath
          root <- Env.getRoot sessionId
          payload <- Viewer.initViewer sessionId root clientPath
          pure $ addHeader payload mempty


  , changeTarget = \mCookie mTargetId ->
      withSession mCookie $ \sessionId -> do
        targetId <- withQueryParam mTargetId
        withServerError $ Env.changeCurrentTarget sessionId targetId
        index sessionId


  , themeCss = \mCookie ->
      withSession mCookie $ \_ -> do
        theme <- Env.getTheme
        dir <- Env.getDataDir
        readFile $
          case theme of
            Dark1 -> dir </> "dark1.css"
            Light1 -> dir </> "light1.css"
  }


runStorage :: _ => Eff (Storage : Error FilehubError : es) a -> Eff es a
runStorage = withServerError . Storage.runStorage


index :: SessionId -> Filehub (Html ())
index sessionId =
  Template.index
  <$> sideBar sessionId
  <*> view sessionId


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = withServerError $
  Template.sideBar
  <$> Env.getTargets
  <*> Target.viewCurrentTarget sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  root <- Env.getRoot sessionId & withServerError
  order <- Env.getSortFileBy sessionId & withServerError
  files <- Domain.sortFiles order <$> (runStorage $ Storage.lsCurrentDir sessionId) & withServerError
  let table = Template.table root files
  Template.view table <$> pathBreadcrumb sessionId


pathBreadcrumb :: SessionId -> Filehub (Html ())
pathBreadcrumb sessionId =
  Template.pathBreadcrumb
  <$> (Env.getCurrentDir sessionId & withServerError)
  <*> (Env.getRoot sessionId & withServerError)
