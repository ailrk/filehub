{-# LANGUAGE PartialTypeSignatures #-}
module Filehub.Server where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock qualified as Time
import Effectful ( MonadIO(liftIO), liftIO, Eff, (:>), IOE, liftIO )
import Effectful.Error.Dynamic (runErrorNoCallStack, throwError, Error)
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import Effectful.Reader.Dynamic (Reader)
import Filehub.ClientPath qualified as ClientPath
import Filehub.Cookie ( getSessionId, Cookies'(..), SetCookie )
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env, TargetView (..))
import Filehub.Env qualified as Env
import Filehub.Env.SessionPool qualified as SessionPool
import Filehub.Env.Target qualified as Target
import Filehub.Error ( withServerError, FilehubError(..), toServerError, FilehubError(..), withServerError )
import Filehub.Monad ( runFilehub, Filehub )
import Filehub.Routes (Api (..))
import Filehub.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Storage (Storage)
import Filehub.Storage qualified as Storage
import Filehub.Template qualified as Template
import Filehub.Types
    ( Target(..),
      ClientPath(..),
      NewFile(..),
      NewFolder(..),
      SortFileBy(..),
      UpdatedFile(..),
      Theme(..),
      Session(..),
      SessionId(..),
      File(..),
      ClientPath(..),
      NewFile(..),
      NewFolder(..),
      SortFileBy(..),
      UpdatedFile(..),
      Theme(..),
      Session(..),
      SessionId(..) )
import Filehub.Viewer qualified as Viewer
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Network.HTTP.Types.Status (mkStatus)
import Network.Wai
import Network.Wai.Application.Static
import Prelude hiding (readFile)
import Prelude hiding (readFile)
import Servant ( Raw, Tagged(..), ServerT, ServerError(..), addHeader, noHeader, ServerError )
import Servant qualified as S
import Servant.Server (err400)
import Servant.Server.Generic (AsServerT)
import System.FilePath ((</>), takeFileName)
import Text.Printf (printf)
import Web.Cookie (parseCookies)


-- | Handle static file access
dynamicRaw :: Env -> Servant.ServerT Servant.Raw m
dynamicRaw env = Servant.Tagged $ \req respond -> do
  let mSessionId = do
        bytes <- lookup "Cookie" (requestHeaders req)
        getSessionId . parseCookies $ bytes
  case mSessionId of
    Just sessionId -> do
      mR <- runFilehub env $ withServerError $ do
        root <- Env.getRoot sessionId
        TargetView target _ _ <- Env.currentTarget sessionId
        case target of
          FileTarget _ -> throughFS root req respond
          S3Target _ -> throughS3 sessionId req respond
      case mR of
        Left err -> respond $ serverErrorToResponse err
        Right r -> pure r
    Nothing -> do
      let err = toServerError InvalidSession
      respond $ serverErrorToResponse err
  where
    serverErrorToResponse :: ServerError -> Response
    serverErrorToResponse err =
      responseLBS
        (mkStatus err.errHTTPCode "")
        err.errHeaders
        err.errBody

    -- File from file systems are simply served as a static app
    throughFS root req respond = liftIO $ do
      let app = staticApp (defaultWebAppSettings root)
      app req respond

    -- File from S3 are forwared from S3 to the client.
    throughS3 sessionId req respond = do
      let path = case ByteString.Char8.unpack req.rawPathInfo of
                   '/':rest -> rest
                   other -> other
      (file, bytes) <- Storage.runStorage sessionId $ do
        file <- Storage.getFile path
        bytes <- Storage.readFileContent file
        pure (file, bytes)
      liftIO $ do
        respond $
          responseLBS
            (mkStatus 200 "")
            [ ("Content-Type", file.mimetype)
            ]
            bytes


server :: Api (AsServerT Filehub)
server = Api
  { index = \mCookie -> withSession mCookie (fmap Template.withDefault . index)


  , cd = \mCookie mClientPath -> do
      withSession mCookie $ \sessionId -> do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId & withServerError
        r <- runStorage sessionId $ Storage.changeDir (ClientPath.fromClientPath root clientPath) & runErrorNoCallStack
        let header = either addHeader (const noHeader) r
        header <$> view sessionId


  , newFile = \mCookie (NewFile path) -> do
      withSession mCookie $ \sessionId -> do
        r <- runStorage sessionId $ Storage.newFile (Text.unpack path) & runErrorNoCallStack
        let header = either addHeader (const noHeader) r
        header <$> view sessionId


  , updateFile = \mCookie (UpdatedFile clientPath content) -> do
      withSession mCookie $ \ sessionId -> do
        let path = clientPath.unClientPath
        _ <- runStorage sessionId $ Storage.writeFile path (Text.encodeUtf8 content ^. lazy)
        view sessionId


  , deleteFile = \mCookie mClientPath ->
      withSession mCookie $ \sessionId -> do
        withServerError do
          clientPath <- withQueryParam mClientPath
          root <- Env.getRoot sessionId
          let p = ClientPath.fromClientPath root clientPath
          runStorage sessionId  $ Storage.deleteFile p
        view sessionId


  , newFolder = \mCookie (NewFolder path) ->
      withSession mCookie $ \sessionId -> do
        r <- runStorage sessionId do
          Storage.newFolder (Text.unpack path) & runErrorNoCallStack
        let header = either addHeader (const noHeader) r
        header <$> view sessionId


  , newFileModal = \mCookie -> withSession mCookie $ const (pure Template.newFileModal)


  , newFolderModal = \mCookie -> withSession mCookie $ const (pure Template.newFolderModal)


  , fileDetailModal = \mCookie mClientPath ->
      withSession mCookie $ \sessionId ->
        withServerError do
          clientPath <- withQueryParam mClientPath
          root <- Env.getRoot sessionId
          file <- runStorage sessionId $ Storage.getFile (ClientPath.fromClientPath root clientPath)
          pure (Template.fileDetailModal file)


  , uploadModal = \mCookie -> withSession mCookie $ const (pure Template.uploadModal)


  , editorModal = \mCookie mClientPath ->
      withSession mCookie $ \sessionId -> do
        withServerError do
          clientPath <- withQueryParam mClientPath
          root <- Env.getRoot sessionId
          let p = ClientPath.fromClientPath root clientPath
          content <- runStorage sessionId do
            f <- Storage.getFile p
            Storage.readFileContent f
          let filename = takeFileName p
          pure $ Template.editorModal filename content


  , search = \mCookie searchWord ->
      withSession mCookie $ \sessionId ->
        withServerError . runStorage sessionId $ do
          TargetView target _ _ <- Env.currentTarget sessionId & withServerError
          root <- Env.getRoot sessionId
          files <- Storage.lsCurrentDir
          order <- Env.getSortFileBy sessionId
          selected <- Selected.getSelected sessionId
          pure $ Template.search searchWord target root files selected order


  , sortTable = \mCookie order -> do
      withSession mCookie $ \sessionId -> do
        Env.setSortFileBy sessionId (fromMaybe ByNameUp order)
        view sessionId


  , selectRows = \mCookie selected -> do
      withSession mCookie $ \sessionId -> do
        Selected.setSelected sessionId selected
        view sessionId


  , upload = \mCookie multipart ->
      withSession mCookie $ \sessionId -> do
        runStorage sessionId $ Storage.upload multipart
        index sessionId


  , download = \mCookie mClientPath ->
      withSession mCookie $ \sessionId -> do
        clientPath@(ClientPath path) <- withQueryParam mClientPath
        bs <- runStorage sessionId $ Storage.download clientPath
        pure $ addHeader (printf "attachement; filename=%s" (takeFileName path)) bs


  , contextMenu = \mCookie mClientPath ->
      withSession mCookie $ \sessionId ->
        withServerError do
          clientPath <- withQueryParam mClientPath
          root <- Env.getRoot sessionId
          let filePath = ClientPath.fromClientPath root clientPath
          file <- runStorage sessionId $ Storage.getFile filePath
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
            Dark -> dir </> "dark.css"
            Light -> dir </> "light.css"

  , healthz = pure "ok"
  }


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


runStorage :: _ => SessionId -> Eff (Storage : Error FilehubError : es) a -> Eff es a
runStorage sessionId = withServerError . Storage.runStorage sessionId


index :: SessionId -> Filehub (Html ())
index sessionId =
  Template.index
  <$> sideBar sessionId
  <*> view sessionId


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = withServerError $
  Template.sideBar
  <$> Env.getTargets
  <*> Target.currentTarget sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  root <- Env.getRoot sessionId & withServerError
  order <- Env.getSortFileBy sessionId & withServerError
  files <- sortFiles order <$> runStorage sessionId Storage.lsCurrentDir & withServerError
  TargetView target _ _ <- Env.currentTarget sessionId & withServerError
  selected <- Selected.getSelected sessionId & withServerError
  let table = Template.table target root files selected order
  Template.view table <$> pathBreadcrumb sessionId


pathBreadcrumb :: SessionId -> Filehub (Html ())
pathBreadcrumb sessionId =
  Template.pathBreadcrumb
  <$> (Env.getCurrentDir sessionId & withServerError)
  <*> (Env.getRoot sessionId & withServerError)
