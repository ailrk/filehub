{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Filehub.Server (application, main, mainDev) where

import Codec.Archive.Zip qualified as Zip
import System.IO.Temp qualified as Temp
import Data.String.Interpolate (i)
import Data.Maybe (fromMaybe)
import Data.Foldable (forM_)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Aeson (object, KeyValue (..), (.:), withObject)
import Data.Aeson.Types (parseMaybe)
import Data.FileEmbed qualified as FileEmbed
import Data.Time (secondsToNominalDiffTime, UTCTime (..), fromGregorian)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Effectful.Log (logAttention_, logInfo_)
import Effectful ( withRunInIO, MonadIO (liftIO) )
import Effectful.Error.Dynamic (throwError)
import Effectful (runEff)
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (runLog)
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Servant.Server.Generic (AsServerT)
import Servant (errBody, Headers, Header, NoContent (..), err404, errHeaders, err301, noHeader, err400)
import Servant (addHeader, err500)
import Servant (serveWithContextT, Context (..), Application)
import Servant.Conduit ()
import System.FilePath (takeFileName, (</>), takeDirectory, makeRelative)
import Control.Exception (SomeException)
import Control.Monad (when, forM, replicateM)
import UnliftIO (catch)
import Text.Printf (printf)
import Filehub.Layout (Layout(..))
import Filehub.Mime (isMime)
import Filehub.Target qualified as Target
import Filehub.Target.Types.TargetView (TargetView(..))
import Filehub.Types ( FilehubEvent (..), LoginForm(..), MoveFile (..), UIComponent (..), FileContent (..))
import Filehub.Env qualified as Env
import Filehub.Error ( withServerError, FilehubError(..), withServerError, Error' (..) )
import Filehub.Routes (Api (..))
import Filehub.Types
    ( SessionId(..), Display (..))
import Filehub.Template.Internal qualified as Template
import Filehub.Template qualified as Template
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.ClientPath qualified as ClientPath
import Filehub.Selected qualified as Selected
import Filehub.Env
import Filehub.Log qualified as Log
import Filehub.Monad
import Filehub.Options (Options(..), parseOptions, TargetOption (..), LoginInfo (..))
import Filehub.Routes qualified as Routes
import Filehub.Server.Handler qualified as Server.Handler
import Filehub.Server.Middleware qualified as Server.Middleware
import Filehub.Server.Desktop qualified as Server.Desktop
import Filehub.Server.Mobile qualified as Server.Mobile
import Filehub.Server.Internal (withQueryParam, clear, copy, paste, parseHeader')
import Filehub.SessionPool qualified as SessionPool
import Filehub.Types (Target(..))
import Filehub.Target.File qualified as FS
import Filehub.Target.S3 qualified as S3
import Filehub.Theme qualified as Theme
import Filehub.Types
    ( File(..),
      ClientPath(..),
      UpdatedFile(..),
      ClientPath(..),
      NewFile(..),
      NewFolder(..),
      SortFileBy(..),
      UpdatedFile(..),
      Theme(..),
      Selected (..))
import Filehub.Viewer qualified as Viewer
import Filehub.ControlPanel qualified as ControlPanel
import Filehub.Storage (getStorage, Storage(..))
import Filehub.Cookie qualified as Cookie
import Conduit (ConduitT, ResourceT)
import Conduit qualified
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (withArgs)
import UnliftIO (hFlush, stdout)
import Network.Mime qualified as Mime
import Filehub.Server.Handler (ConfirmLogin)
import Filehub.User qualified as User
import Filehub.User (Username(..))
import Filehub.Cookie qualified as Cookies
import Web.Cookie (SetCookie (..))
import Network.HTTP.Types.Header (hLocation)
import Effectful.Reader.Dynamic (asks)
import System.Directory (removeFile)
import System.Random (randomRIO)


#ifdef DEBUG
import Effectful ( MonadIO (liftIO) )
import System.FilePath ((</>))
import Paths_filehub qualified
import System.Directory (makeAbsolute)
import Data.ByteString (readFile)
#endif

-- | Server definition
server :: Api (AsServerT Filehub)
server = Api
  { init = \sessionId res -> do
      Env.updateSession sessionId $
        \s -> s & #resolution .~ Just res
      clear sessionId
      index sessionId


  -- Only the top level index will load resources (js, css, etc) if display is valid.
  -- Whenver you need to re-render the index page, call the `index` free function instead,
  -- which only render the element #index.
  --
  -- `bootstrap` is used to query the device resolution before rendering anything. Once
  -- the session is bootstrapped, display information will be available for all subsequent
  -- requests.
  --
  -- The frontend js deletes the `display` cookie on `pageunload`, so the backend can
  -- start a full reload from the bootstrap stage.
  , index = \sessionId _ -> do
      display <- Env.getDisplay sessionId & withServerError
      manifest <- server.manifest
      let background
            = fromMaybe "#000000"
            $ flip parseMaybe manifest
            $ withObject "manifest"
            $ (.: "theme_color")

      clear sessionId
      case display of
        NoDisplay -> pure Template.bootstrap
        Desktop -> fmap (Template.withDefault display background) $ Server.Desktop.index sessionId
        Mobile -> fmap (Template.withDefault display background) $ Server.Mobile.index sessionId


  , refresh = \sessionId _ mUIComponent -> do
      case mUIComponent of
        Just UIComponentContronPanel -> do
          controlPanel sessionId
        Just UIComponentSideBar -> do
          sideBar sessionId
        Just UIComponentView -> do
          view sessionId
        Nothing ->
          throwError (err400 { errBody = [i|Invalid ui component|]})


  , login = login


  , loginPost = loginPost


  , logout = logout


  , cd = \sessionId _ mClientPath -> do
      clientPath <- withQueryParam mClientPath
      withServerError do
        root <- Env.getRoot sessionId
        storage <- getStorage sessionId
        storage.cd (ClientPath.fromClientPath root clientPath)
      view sessionId <&> addHeader DirChanged


  , newFile = \sessionId _ _ (NewFile path) -> do
      withServerError do
        storage <- getStorage sessionId
        storage.new (Text.unpack path)
      view sessionId


  , updateFile = \sessionId _ _ (UpdatedFile clientPath content) -> do
      let path = clientPath.unClientPath
      withServerError do
        storage <- getStorage sessionId
        storage.write path (Text.encodeUtf8 content)
      view sessionId


  , deleteFile = \sessionId _ _ clientPaths deleteSelected -> do
      withServerError do
        storage <- getStorage sessionId
        root <- Env.getRoot sessionId
        forM_ clientPaths $ \clientPath -> do
          let path = ClientPath.fromClientPath root clientPath
          storage.delete path

        when deleteSelected do
          allSelecteds <- Selected.allSelecteds sessionId
          forM_ allSelecteds $ \(target, selected) -> do
            Target.withTarget sessionId (Target.getTargetId target) $ \_ -> do
              case selected of
                NoSelection -> pure ()
                Selected x xs -> do
                  forM_ (fmap (ClientPath.fromClientPath root) (x:xs)) $ \path -> do
                    storage.delete path
      clear sessionId
      count <- Selected.countSelected sessionId & withServerError
      addHeader count <$> index sessionId


  , newFolder = \sessionId _ _ (NewFolder path) -> do
      withServerError do
        storage <- getStorage sessionId
        storage.newFolder (Text.unpack path)
      view sessionId


  , newFileModal = \_ _ _ _ -> pure Template.Desktop.newFileModal


  , newFolderModal = \_ _ _ _ -> pure Template.Desktop.newFolderModal


  , fileDetailModal = \sessionId _ -> Server.Desktop.fileDetailModal sessionId


  , editorModal = \sessionId _ mClientPath -> do
      display <- Env.getDisplay sessionId & withServerError
      case display of
        Mobile -> Server.Mobile.editorModal sessionId mClientPath
        Desktop -> Server.Desktop.editorModal sessionId mClientPath
        NoDisplay -> undefined


  , search = \sessionId _ searchWord -> withServerError do
      display <- Env.getDisplay sessionId
      storage <- getStorage sessionId
      TargetView target _ _ <- Env.currentTarget sessionId
      root <- Env.getRoot sessionId
      files <- storage.lsCwd
      order <- Env.getSortFileBy sessionId
      layout <- Env.getLayout sessionId
      selected <- Selected.getSelected sessionId
      case display of
        Mobile -> pure $ Template.Mobile.search searchWord target root files selected order
        Desktop -> pure $ Template.Desktop.search searchWord target root files selected order layout
        NoDisplay -> undefined


  , sortTable = \sessionId _ order -> do
      Env.setSortFileBy sessionId (fromMaybe ByNameUp order)
      addHeader TableSorted <$> view sessionId


  , selectLayout = \sessionId _ layout -> do
      Env.setLayout sessionId (fromMaybe ThumbnailLayout layout)
      addHeader LayoutChanged <$> index sessionId


  , selectRows = \sessionId _ selected -> do
      case selected of
        NoSelection -> do
          logAttention_ [i|No selection: #{sessionId}|]
          throwError (FilehubError InvalidSelection "Nothing is selected") & withServerError
        _ -> do
          Selected.setSelected sessionId selected
          count <- Selected.countSelected sessionId & withServerError
          addHeader count <$> controlPanel sessionId


  , upload = \sessionId _ _ multipart -> do
      withServerError do
        storage <- getStorage sessionId
        storage.upload multipart
      index sessionId


  , download = \sessionId _ clientPaths -> do
      storage <- getStorage sessionId & withServerError
      root <- Env.getRoot sessionId & withServerError

      case clientPaths of
        [clientPath@(ClientPath path)] -> do
          file <- storage.get (ClientPath.fromClientPath root clientPath) & withServerError
          conduit <- withServerError $ storage.download clientPath
          let filename =
                case file.content of
                  Content -> printf "attachement; filename=%s" (takeFileName path)
                  Dir _ -> printf "attachement; filename=%s.zip" (takeFileName path)
          pure $ addHeader filename conduit

        _ -> do
          (zipPath, _) <- liftIO do
            tempDir <- Temp.getCanonicalTemporaryDirectory
            Temp.openTempFile tempDir "DXXXXXX.zip"

          tasks <-
            forM (fmap (ClientPath.fromClientPath root) clientPaths) $ \path -> withServerError do
              file <- storage.get path
              conduit <- storage.readStream file
              pure (path, conduit)


          Zip.createArchive zipPath $ do
            forM_ tasks $ \(path, conduit) -> do
              m <- Zip.mkEntrySelector  (makeRelative root path)
              Zip.sinkEntry Zip.Zstd conduit m
          tag <- Text.pack <$> replicateM 8 (randomRIO ('a', 'z'))
          let conduit =
                Conduit.bracketP
                  (pure ())
                  (\_ -> liftIO $ removeFile zipPath)
                  (\_ -> Conduit.sourceFile zipPath)

          pure $ addHeader (printf "attachement; filename=%s.zip" tag) conduit


  , copy = \sessionId _ _ -> do
      copy sessionId
      controlPanel sessionId


  , paste = \sessionId _ _ -> do
      paste sessionId
      clear sessionId
      count <- Selected.countSelected sessionId & withServerError
      addHeader count <$> index sessionId


  , move = \sessionId _ _ (MoveFile src tgt) -> do
      root <- Env.getRoot sessionId & withServerError
      storage <- getStorage sessionId & withServerError
      let srcPaths = fmap (ClientPath.fromClientPath root) src
      let tgtPath = ClientPath.fromClientPath root tgt

      -- check before take action
      forM_ srcPaths $ \srcPath -> withServerError do
        isTgtDir <- storage.isDirectory tgtPath
        when (not isTgtDir) do
          throwError $ FilehubError InvalidDir "Target is not a directory"

        when (srcPath == tgtPath)  do
          throwError $ FilehubError InvalidDir "Can't move to the same directory"

        when (takeDirectory srcPath == tgtPath)  do
          throwError $ FilehubError InvalidDir "Already in the current directory"

      forM_ srcPaths $ \srcPath -> withServerError do
        let fileName = takeFileName srcPath
        storage.cp srcPath (tgtPath </> fileName)
        storage.delete srcPath
      addHeader FileMoved <$> index sessionId


  , cancel = \sessionId _ -> do
      clear sessionId
      count <- Selected.countSelected sessionId & withServerError
      addHeader count <$> index sessionId


  , contextMenu = \sessionId _ -> Server.Desktop.contextMenu sessionId


  , initViewer = \sessionId _ mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        payload <- Viewer.initViewer sessionId root clientPath
        pure $ addHeader payload NoContent


  , open = \_ _ mTarget mClientPath -> do
      clientPath <- withQueryParam mClientPath
      target <- withQueryParam mTarget
      pure $ addHeader (Opened target clientPath) NoContent


  , changeTarget = \sessionId _ mTargetId -> do
      savedTargetId <- withServerError do
        TargetView saved _ _ <- Target.currentTarget sessionId
        pure $ Target.getTargetId saved

      let restore = Env.changeCurrentTarget sessionId savedTargetId & withServerError
      targetId <- withQueryParam mTargetId
      Env.changeCurrentTarget sessionId targetId & withServerError

      html <- withRunInIO $ \unlift -> do
        unlift (index sessionId) `catch` \(_ :: SomeException) -> unlift do
          restore
          throwError (err500 { errBody = [i|Invalid target|]})

      pure $ addHeader TargetChanged html


  , themeCss = \sessionId -> do
#ifdef DEBUG
      theme <- Env.getSessionTheme sessionId & withServerError
      dir <- liftIO $ Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data/filehub")
      liftIO . readFile $
        case theme of
          Dark -> dir </> "theme-dark.css"
          Light -> dir </> "theme-light.css"
#else
      theme <- Env.getSessionTheme sessionId & withServerError
      pure
        case theme of
          Dark -> fromMaybe "no-theme" $ Map.lookup "theme-dark.css" staticFiles
          Light -> fromMaybe "no-theme" $ Map.lookup "theme-light.css" staticFiles
#endif


  , toggleTheme = \sessionId _ -> do
      theme <- Env.getSessionTheme sessionId & withServerError
      case theme of
        Theme.Light -> Env.setSessionTheme sessionId Theme.Dark
        Theme.Dark -> Env.setSessionTheme sessionId Theme.Light
      addHeader ThemeChanged <$> index sessionId


  , serve = serve


  , thumbnail = thumbnail


  , manifest = do
      let t = Text.pack
      pure $
        object
          [ "name"       .= t "FileHub"
          , "short_name" .= t "FileHub"
          , "start_url"  .= t "/"
          , "display"    .= t "standalone"
          , "icons" .=
              [ object
                    [ "src"     .= t "/static/web-app-manifest-192x192.png"
                    , "sizes"   .= t "192x192"
                    , "type"    .= t "image/png"
                    , "purpose" .= t "any"
                    ]
              , object
                    [ "src"     .= t "/static/web-app-manifest-512x512.png"
                    , "sizes"   .= t "512x512"
                    , "type"    .= t "image/png"
                    , "purpose" .= t "maskable"
                    ]
              ]
          ]


  , favicon = pure $(FileEmbed.embedFile "data/filehub/favicon.ico")


  , static = \paths -> do
#ifdef DEBUG
      dir <- liftIO $ Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data/filehub")
      let path = dir </> List.intercalate "/" paths
      let mimetype = Mime.defaultMimeLookup (Text.pack path)
      content <- liftIO . readFile $ path
      pure
        $ addHeader (ByteString.unpack mimetype)
        $ content
#else
      let path = List.intercalate "/" paths
      case Map.lookup path staticFiles of
        Just content -> do
          let mimetype = Mime.defaultMimeLookup (Text.pack path)
          pure
            $ addHeader (ByteString.unpack mimetype)
            $ content
        Nothing -> throwError (err404 { errBody = [i|File doesn't exist|]})
#endif


  , offline = pure Template.offline


  , healthz = pure "ok"


#ifdef DEBUG
  , debug1 = \_ -> pure $ addHeader (Dummy "Hello") NoContent
#endif
  }


-- | Static files are embeded into the final excutable. The key is the path of the file.
--   e.g ui.js -> (data/filehub/ui.js)
staticFiles :: Map FilePath ByteString
staticFiles = Map.fromList
  $(FileEmbed.embedDir "data/filehub")


-- | index that reset the state
index :: SessionId -> Filehub (Html ())
index sessionId = do
  display <- Env.getDisplay sessionId & withServerError
  case display of
    NoDisplay -> pure Template.bootstrap
    Desktop -> Server.Desktop.index sessionId
    Mobile -> Server.Mobile.index sessionId


login :: SessionId -> Maybe Text -> Filehub (Html ())
login sessionId cookie = do
  noLogin <- asks @Env (.noLogin)
  if noLogin
     then go
     else do
       case fmap Text.encodeUtf8 cookie >>= parseHeader' >>= Cookies.getAuthId of
         Just authId' -> do
           authId <- Env.getAuthId sessionId & withServerError
           if authId == Just authId'
              then go
              else pure Template.login
         Nothing -> pure Template.login
  where
    go = throwError (err301 { errHeaders = [(hLocation, "/")] })


loginPost :: SessionId -> LoginForm
          -> Filehub (Headers '[ Header "Set-Cookie" SetCookie
                               , Header "HX-Redirect" Text
                               ] (Html ()))
loginPost sessionId (LoginForm username password) =  do
  db <- Env.getUserDB
  if User.validate (Username username) (Text.encodeUtf8 password) db then do
    User.createAuthId >>= Env.setAuthId sessionId . Just
    session <- Env.getSession sessionId & withServerError
    case Cookie.setAuthId session of
      Just setCookie -> do
        logInfo_ [i|User #{username} logged in|]
        addHeader setCookie . addHeader "/" <$> pure mempty
      Nothing -> do noHeader . noHeader <$> pure Template.loginFailed
  else do noHeader . noHeader <$> pure Template.loginFailed



logout :: SessionId -> ConfirmLogin -> Filehub (Headers '[ Header "Set-Cookie" SetCookie
                                                         , Header "HX-Redirect" Text
                                                         ] (Html ()))
logout sessionId _ = do
  session <- Env.getSession sessionId & withServerError
  case Cookie.setAuthId session of
    Just setCookie -> do
      Env.setAuthId sessionId Nothing
      addHeader
        (setCookie
          { setCookieExpires = Just (UTCTime (fromGregorian 1970 1 1) 0) })
        . addHeader "/login"
        <$> pure mempty

    Nothing -> noHeader . noHeader <$> pure mempty


view :: SessionId -> Filehub (Html ())
view sessionId = do
  display <- Env.getDisplay sessionId & withServerError
  case display of
    Desktop -> Server.Desktop.view sessionId
    Mobile -> Server.Mobile.view sessionId
    NoDisplay -> Server.Mobile.view sessionId


controlPanel :: SessionId -> Filehub (Html ())
controlPanel sessionId = withServerError do
  display <- Env.getDisplay sessionId
  theme <- Env.getSessionTheme sessionId
  layout <- Env.getLayout sessionId
  readOnly <- Env.getReadOnly
  noLogin <- Env.getNoLogin
  state <- ControlPanel.getControlPanelState sessionId
  pure $
    case display of
      Desktop -> Template.Desktop.controlPanel layout theme readOnly noLogin state
      Mobile -> Template.Mobile.controlPanel theme readOnly noLogin state
      NoDisplay -> Template.Mobile.controlPanel theme readOnly noLogin state


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  display <- Env.getDisplay sessionId & withServerError
  case display of
    Desktop -> Server.Desktop.sideBar sessionId
    _ -> error "impossible"


serve :: SessionId -> ConfirmLogin -> Maybe ClientPath
      -> Filehub (Headers '[ Header "Content-Type" String
                           , Header "Content-Disposition" String
                           ] (ConduitT () ByteString (ResourceT IO) ()))
serve sessionId _ mFile = do
  withServerError do
    storage <- getStorage sessionId
    root <- Env.getRoot sessionId
    clientPath <- withQueryParam mFile
    let path = ClientPath.fromClientPath root clientPath
    file <- storage.get path
    conduit <- storage.readStream file
    pure
      $ addHeader (ByteString.unpack file.mimetype)
      $ addHeader (printf "inline; filename=%s" (takeFileName path))
      $ conduit


thumbnail :: SessionId -> ConfirmLogin -> Maybe ClientPath
          -> Filehub (Headers '[ Header "Content-Type" String
                               , Header "Content-Disposition" String
                               ] (ConduitT () ByteString (ResourceT IO) ()))
thumbnail sessionId _ mFile = do
  withServerError do
    storage <- getStorage sessionId
    root <- Env.getRoot sessionId
    clientPath <- withQueryParam mFile
    let path = ClientPath.fromClientPath root clientPath
    file <- storage.get path
    conduit <- serveOriginal storage file
    pure
      $ addHeader (ByteString.unpack file.mimetype)
      $ addHeader (printf "inline; filename=%s" (takeFileName path))
      $ conduit
  where
    serveOriginal storage file =
      if
        | file.mimetype `isMime` "image" ->
          storage.readStream file
        | otherwise -> throwError (FilehubError FormatError "Invalid mime type for thumbnail") & withServerError


application :: Env -> Application
application env
  = Server.Middleware.exposeHeaders
  . Server.Middleware.sessionMiddleware env
  . Server.Middleware.dedupHeadersKeepLast
  . Server.Middleware.displayMiddleware env
  . serveWithContextT Routes.api ctx (Server.Handler.toServantHandler env)
  $ server
  where

    ctx = Server.Handler.sessionHandler env
        :. Server.Handler.readOnlyHandler env
        :. Server.Handler.desktopOnlyHandler env
        :. Server.Handler.mobileOnlyHandler env
        :. Server.Handler.loginHandler env
        :. EmptyContext

------------------------------------
-- main
------------------------------------


main :: IO ()
main = Log.withColoredStdoutLogger \logger -> do
  options <- parseOptions
  sessionPool <- runEff SessionPool.new
  targets <- runEff . runLog "Targets" logger options.verbosity . runFileSystem $ fromTargetOptions options.targets
  userDB <- runEff . runFileSystem $ User.createUserDB options.loginInfo

  printf "PORT: %d\n" options.port
  printf "V: %s\n" (show options.verbosity)
#ifdef DEBUG
  printf "DEBUG build\n"
#endif

  let env =
        Env
          { port = options.port
          , theme = options.theme
          , sessionPool = sessionPool
          , sessionDuration = secondsToNominalDiffTime (60 * 60)
          , targets = targets
          , readOnly = options.readOnly
          , logger = logger
          , logLevel = options.verbosity
          , userDB = userDB
          , noLogin = case options.loginInfo of
                        NoLogin -> True
                        _ -> False
          }
  go env `catch` handler
  where
    go env = do
      putStr "[Filehub server is up and running]\n" >> hFlush stdout
      let settings = setPort env.port defaultSettings
      runSettings settings . logStdout $ application env
    handler (e :: SomeException) = putStrLn ("server is down " <> show e) >> hFlush stdout

    fromTargetOptions opts = traverse transform opts
      where
        transform (FSTargetOption opt) = Target <$> FS.initialize opt
        transform (S3TargetOption opt) = Target <$> S3.initialize opt


-- | For developement with ghciwatch
--   Run `ghciwatch --test-ghci "Filehub.Entry.mainDev <your args for testing>"`
--   ghciwatch will watch file changes and rerun the server automatically.
mainDev :: String -> IO ()
mainDev args = withArgs (words args) main
