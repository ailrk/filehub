{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server
  ( server
  , dynamicRaw
  , sessionMiddleware
  ) where

import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.String.Interpolate (i)
import Data.UUID qualified as UUID
import Effectful ( MonadIO(liftIO), liftIO, liftIO, runEff )
import Effectful.Reader.Dynamic (runReader)
import Effectful.Log (logAttention_)
import Effectful ( withRunInIO )
import Effectful.Error.Dynamic (throwError)
import Filehub.Target qualified as Target
import Filehub.Types
    ( FilehubEvent (..))
import Filehub.Cookie ( getSessionId, parseCookies )
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env (..), TargetView (..))
import Filehub.Env qualified as Env
import Filehub.SessionPool qualified as SessionPool
import Filehub.Error ( withServerError, FilehubError(..), toServerError, FilehubError(..), withServerError )
import Filehub.Monad ( runFilehub, Filehub )
import Filehub.Routes (Api (..))
import Filehub.Storage qualified as Storage
import Filehub.Types
    ( Target(..),
      Session(..),
      SessionId(..),
      File(..),
      Session(..),
      SessionId(..), Display (..), Resolution (..))
import Filehub.Server.Desktop qualified as Server.Desktop
import Filehub.Server.Mobile qualified as Server.Mobile
import Filehub.Template qualified as Template
import Lens.Micro
import Lens.Micro.Platform ()
import Network.HTTP.Types.Status (mkStatus)
import Network.Wai
import Network.Wai.Application.Static
import Prelude hiding (readFile)
import Servant ( Raw, Tagged(..), ServerT, ServerError(..), ServerError)
import Servant.Server.Generic (AsServerT)
import Log (runLogT, logTrace_)
import Network.URI.Encode qualified as URI
import Control.Exception (SomeException)
import Lens.Micro.Platform ()
import Prelude hiding (readFile)
import Prelude hiding (readFile)
import Servant ( addHeader, err500 )
import UnliftIO (catch)
import Lucid

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Foldable (forM_)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import Filehub.ClientPath qualified as ClientPath
import Filehub.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Server.Internal (withQueryParam, runStorage, clear, parseHeader')
import Filehub.Types
    ( ClientPath(..),
      NewFile(..),
      NewFolder(..),
      SortFileBy(..),
      UpdatedFile(..),
      Theme(..),
      ClientPath(..),
      NewFile(..),
      NewFolder(..),
      SortFileBy(..),
      UpdatedFile(..),
      Theme(..),
      Selected (..))
import Filehub.Viewer qualified as Viewer
import Lens.Micro.Platform ()
import Prelude hiding (readFile)
import Prelude hiding (readFile)
import System.FilePath ((</>), takeFileName)
import Text.Printf (printf)


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
        file <- Storage.get path
        bytes <- Storage.read file
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
  let mSessionId = mCookie >>= parseHeader' >>= Cookies.getSessionId
  case mSessionId of
    Just sessionId -> do
      eSession <- liftIO . runFilehub env $ SessionPool.getSession sessionId & withServerError
      case eSession of
        Left _ -> do
          logTrace_ [i|Invalid session: #{sessionId}|]
          respondWithNewSession
        Right _ -> do
          logTrace_ [i|Existed session, #{sessionId} |]
          liftIO $ app req respond
    Nothing -> do
      logTrace_ [i|No session found.|]
      respondWithNewSession
  where
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


-- | Server definition
server :: Api (AsServerT Filehub)
server = Api
  { init = \sessionId mRes -> do
      case mRes of
        Just res -> do
          Env.updateSession sessionId $
            \s -> s & #resolution .~ Just res
          server.index sessionId
        Nothing -> do
          logAttention_ "No resolution info from /init. Set to 360x800 as default"
          Env.updateSession sessionId $
            \s -> s & #resolution .~ Just (Resolution 360 800)
          server.index sessionId


  , index = \sessionId -> do
      Env.getDisplay sessionId & withServerError >>= \case
        NoDisplay -> pure Template.bootstrap
        Desktop -> Server.Desktop.index sessionId
        Mobile -> Server.Mobile.index sessionId


  , cd = \sessionId mClientPath -> do
      clientPath <- withQueryParam mClientPath
      root <- Env.getRoot sessionId & withServerError
      runStorage sessionId $ Storage.cd (ClientPath.fromClientPath root clientPath) & withServerError
      view sessionId <&> addHeader DirChanged


  , newFile = \sessionId _ (NewFile path) -> do
      runStorage sessionId $ Storage.new (Text.unpack path) & withServerError
      view sessionId


  , updateFile = \sessionId _ (UpdatedFile clientPath content) -> do
      let path = clientPath.unClientPath
      _ <- runStorage sessionId $ Storage.write path (Text.encodeUtf8 content ^. lazy)
      view sessionId


  , deleteFile = \sessionId _ mClientPath deleteSelected -> do
      withServerError do
        root <- Env.getRoot sessionId

        when (isJust mClientPath) do
          clientPath <- withQueryParam mClientPath
          let p = ClientPath.fromClientPath root clientPath
          runStorage sessionId  $ Storage.delete p

        when deleteSelected do
          allSelecteds <- Selected.allSelecteds sessionId
          forM_ allSelecteds $ \(target, selected) -> do
            Target.withTarget sessionId (Target.getTargetId target) do
              case selected of
                NoSelection -> pure ()
                Selected x xs -> do
                  forM_ (fmap (ClientPath.fromClientPath root) (x:xs)) $ \path -> do
                    runStorage sessionId  $ Storage.delete path
          clear sessionId
      server.index sessionId


  , newFolder = \sessionId _ (NewFolder path) -> do
      runStorage sessionId $ Storage.newFolder (Text.unpack path) & withServerError
      view sessionId


  , newFileModal = \_ _ -> pure Template.Desktop.newFileModal


  , newFolderModal = \_ _ -> pure Template.Desktop.newFolderModal


  , fileDetailModal = Server.Desktop.fileDetailModal


  , editorModal = Server.Desktop.editorModal


  , search = \sessionId searchWord -> do
      withServerError . runStorage sessionId $ do
        TargetView target _ _ <- Env.currentTarget sessionId & withServerError
        root <- Env.getRoot sessionId
        files <- Storage.lsCwd
        order <- Env.getSortFileBy sessionId
        selected <- Selected.getSelected sessionId
        pure $ Template.Desktop.search searchWord target root files selected order


  , sortTable = \sessionId order -> do
      Env.setSortFileBy sessionId (fromMaybe ByNameUp order)
      addHeader TableSorted <$> view sessionId


  , selectRows = Server.Desktop.selectRows


  , upload = \sessionId _ multipart -> do
      runStorage sessionId $ Storage.upload multipart
      server.index sessionId


  , download = \sessionId mClientPath -> do
      clientPath@(ClientPath path) <- withQueryParam mClientPath
      bs <- runStorage sessionId $ Storage.download clientPath
      pure $ addHeader (printf "attachement; filename=%s" (takeFileName path)) bs


  , copy = Server.Desktop.copy


  , paste = Server.Desktop.paste


  , cancel = \sessionId -> do
      clear sessionId
      server.index sessionId


  , contextMenu = Server.Desktop.contextMenu


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
        unlift (server.index sessionId) `catch` \(_ :: SomeException) -> unlift do
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


view :: SessionId -> Filehub (Html ())
view sessionId = do
  root <- Env.getRoot sessionId & withServerError
  order <- Env.getSortFileBy sessionId & withServerError
  files <- sortFiles order <$> runStorage sessionId Storage.lsCwd & withServerError
  TargetView target _ _ <- Env.currentTarget sessionId & withServerError
  selected <- Selected.getSelected sessionId & withServerError
  let table = Template.Desktop.table target root files selected order
  Template.Desktop.view table <$> Server.Desktop.pathBreadcrumb sessionId
