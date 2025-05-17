{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Maybe (fromMaybe, isJust)
import Data.Foldable (forM_)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.String.Interpolate (i)
import Data.UUID qualified as UUID
import Effectful ( MonadIO(liftIO), liftIO, Eff, (:>), IOE, liftIO, runEff, withRunInIO )
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import Effectful.Reader.Dynamic (Reader, runReader)
import Filehub.ClientPath qualified as ClientPath
import Filehub.Cookie ( getSessionId )
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env (..), TargetView (..))
import Filehub.Env qualified as Env
import Filehub.SessionPool qualified as SessionPool
import Filehub.Target qualified as Target
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
      SessionId(..),
      FilehubEvent (..),
      Selected (..))
import Filehub.Viewer qualified as Viewer
import Filehub.ControlPanel qualified as ControlPanel
import Filehub.Copy qualified as Copy
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Network.HTTP.Types.Status (mkStatus)
import Network.Wai
import Network.Wai.Application.Static
import Prelude hiding (readFile)
import Prelude hiding (readFile)
import Servant ( Raw, Tagged(..), ServerT, ServerError(..), addHeader, ServerError, FromHttpApiData (..), err500 )
import Servant.Server (err400)
import Servant.Server.Generic (AsServerT)
import System.FilePath ((</>), takeFileName)
import Text.Printf (printf)
import Web.Cookie (parseCookies)
import Log (runLogT, logTrace_, logAttention_)
import UnliftIO (catch, SomeException)
import Network.URI.Encode qualified as URI


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
      let path = URI.decode
                $ case ByteString.Char8.unpack req.rawPathInfo of
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


-- | If session is not present, create a new session
sessionMiddleware :: Env -> Middleware
sessionMiddleware env@Env{ logger, logLevel } app req respond = runLogT "sessionMiddleware" logger logLevel $ do
  let mCookie = lookup "Cookie" $ requestHeaders req
  case mCookie >>= parseHeader' >>= Cookies.getSessionId' of
    Just sessionId -> do
      eSession <- liftIO . runFilehub env $ SessionPool.getSession sessionId & withServerError
      case eSession of
        Left _ -> do
          logTrace_ [i|Invalid session: #{sessionId}|]
          respondWithNewSession
        Right _ -> do
          logTrace_ [i|Existed session, #{sessionId}|]
          liftIO $ app req respond
    Nothing -> do
      logTrace_ [i|No session found.|]
      respondWithNewSession
  where
    parseHeader' x = either (const Nothing) Just $ parseHeader x
    respondWithNewSession = do
      session <- liftIO . runEff . runReader env $ SessionPool.newSession
      let sessionId@(SessionId sid) = session.sessionId
      let setCookieHeader = ("Set-Cookie", Cookies.renderSetCookie $ Cookies.setSessionId session)
      let injectedCookieHeader = ("Cookie", "sessionId=" <> UUID.toASCIIBytes sid)
      let req' = req { requestHeaders = injectedCookieHeader : requestHeaders req }
      logTrace_ [i|New session: #{sessionId}|]
      liftIO $ app req' $ \res ->
        let res' = mapResponseHeaders (setCookieHeader :) res
         in respond res'


server :: Api (AsServerT Filehub)
server = Api
  { index = fmap Template.withDefault . index'


  , cd = \sessionId mClientPath -> do
      clientPath <- withQueryParam mClientPath
      root <- Env.getRoot sessionId & withServerError
      runStorage sessionId $ Storage.changeDir (ClientPath.fromClientPath root clientPath) & withServerError
      view sessionId <&> addHeader DirChanged


  , newFile = \sessionId (NewFile path) -> do
      runStorage sessionId $ Storage.newFile (Text.unpack path) & withServerError
      view sessionId


  , updateFile = \sessionId (UpdatedFile clientPath content) -> do
      let path = clientPath.unClientPath
      _ <- runStorage sessionId $ Storage.writeFile path (Text.encodeUtf8 content ^. lazy)
      view sessionId


  , deleteFile = \sessionId mClientPath deleteSelected -> do
      withServerError do
        root <- Env.getRoot sessionId

        when (isJust mClientPath) do
          clientPath <- withQueryParam mClientPath
          let p = ClientPath.fromClientPath root clientPath
          runStorage sessionId  $ Storage.deleteFile p

        when deleteSelected do
          allSelecteds <- Selected.allSelecteds sessionId
          forM_ allSelecteds $ \(target, selected) -> do
            Target.withTarget sessionId (Target.getTargetId target) do
              case selected of
                NoSelection -> pure ()
                Selected x xs -> do
                  forM_ (fmap (ClientPath.fromClientPath root) (x:xs)) $ \path -> do
                    runStorage sessionId  $ Storage.deleteFile path
          clear sessionId
      index sessionId


  , newFolder = \sessionId (NewFolder path) -> do
      runStorage sessionId $ Storage.newFolder (Text.unpack path) & withServerError
      view sessionId


  , newFileModal = \_ -> pure Template.newFileModal


  , newFolderModal = \_ -> pure Template.newFolderModal


  , fileDetailModal = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        file <- runStorage sessionId $ Storage.getFile (ClientPath.fromClientPath root clientPath)
        pure (Template.fileDetailModal file)


  , editorModal = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        let p = ClientPath.fromClientPath root clientPath
        content <- runStorage sessionId do
          f <- Storage.getFile p
          Storage.readFileContent f
        let filename = takeFileName p
        pure $ Template.editorModal filename content


  , search = \sessionId searchWord -> do
      withServerError . runStorage sessionId $ do
        TargetView target _ _ <- Env.currentTarget sessionId & withServerError
        root <- Env.getRoot sessionId
        files <- Storage.lsCurrentDir
        order <- Env.getSortFileBy sessionId
        selected <- Selected.getSelected sessionId
        pure $ Template.search searchWord target root files selected order


  , sortTable = \sessionId order -> do
      Env.setSortFileBy sessionId (fromMaybe ByNameUp order)
      addHeader TableSorted <$> view sessionId


  , selectRows = \sessionId selected -> do
      case selected of
        NoSelection -> do
          logAttention_ [i|No selection: #{sessionId}|]
          throwError InvalidSelection & withServerError
        _ -> do
          Selected.setSelected sessionId selected
          Template.controlPanel <$> ControlPanel.getControlPanelState sessionId & withServerError


  , upload = \sessionId multipart -> do
      runStorage sessionId $ Storage.upload multipart
      index sessionId


  , download = \sessionId mClientPath -> do
      clientPath@(ClientPath path) <- withQueryParam mClientPath
      bs <- runStorage sessionId $ Storage.download clientPath
      pure $ addHeader (printf "attachement; filename=%s" (takeFileName path)) bs


  , copy = \sessionId ->
      withServerError do
        Copy.select sessionId
        Copy.copy sessionId
        Template.controlPanel <$> ControlPanel.getControlPanelState sessionId


  , paste = \sessionId -> do
      withRunInIO $ \unlift -> do
        unlift (Copy.paste sessionId & withServerError) `catch` \(_ :: SomeException) -> unlift do
          throwError (err500 { errBody = [i|Paste failed|]})
      index sessionId


  , cancel = \sessionId -> do
      clear sessionId
      index sessionId


  , contextMenu = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        let filePath = ClientPath.fromClientPath root clientPath
        file <- runStorage sessionId $ Storage.getFile filePath
        pure $ Template.contextMenu root file


  , initViewer = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        payload <- Viewer.initViewer sessionId root clientPath
        pure $ addHeader payload mempty


  , changeTarget = \sessionId mTargetId -> do
      savedTargetId <- do
        TargetView saved _ _ <- Target.currentTarget sessionId & withServerError
        pure $ Target.getTargetId saved
      let restore = Env.changeCurrentTarget sessionId savedTargetId & withServerError
      targetId <- withQueryParam mTargetId
      Env.changeCurrentTarget sessionId targetId & withServerError
      html <- withRunInIO $ \unlift -> do
        unlift (index sessionId) `catch` \(_ :: SomeException) -> unlift do
          restore
          throwError (err500 { errBody = [i|Invalid target|]})
      pure $ addHeader TargetChanged html


  , themeCss = do
      theme <- Env.getTheme
      dir <- Env.getDataDir
      readFile $
        case theme of
          Dark -> dir </> "dark.css"
          Light -> dir </> "light.css"


  , healthz = pure "ok"
  }


withQueryParam :: (Error ServerError :> es) => Maybe a -> Eff es a
withQueryParam m =
  case m of
    Just a -> pure a
    Nothing -> throwError err400


runStorage :: _ => SessionId -> Eff (Storage : Error FilehubError : es) a -> Eff es a
runStorage sessionId = withServerError . Storage.runStorage sessionId


-- | Hard reset all session data. This inclues the current selection, copy-paste status, etc.
index' :: SessionId -> Filehub (Html ())
index' sessionId = do
  clear sessionId
  index sessionId


index :: SessionId -> Filehub (Html ())
index sessionId =
  Template.index
  <$> sideBar sessionId
  <*> view sessionId
  <*> (ControlPanel.getControlPanelState sessionId & withServerError)


clear :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clear sessionId = do
  Selected.clearSelectedAllTargets sessionId
  Copy.clearCopyState sessionId


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
