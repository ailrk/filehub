{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Filehub.Server (application, main, mainDev) where

import Codec.Picture.STBIR qualified as Picture.STBIR
import Codec.Picture qualified as Picture
import Data.String.Interpolate (i)
import Data.Maybe (fromMaybe, isJust)
import Data.Foldable (forM_)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Aeson (object, KeyValue (..), (.:), withObject)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed qualified as FileEmbed
import Data.Time (secondsToNominalDiffTime)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Effectful.Log (logAttention_)
import Effectful ( withRunInIO )
import Effectful.Error.Dynamic (throwError)
import Effectful (runEff)
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (runLog)
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Servant.Server.Generic (AsServerT)
import Servant (errBody, Headers, Header, NoContent (..), err404)
import Servant (addHeader, err500)
import Servant (serveWithContextT, Context (..), Application)
import Servant.Conduit ()
import System.FilePath (takeFileName)
import Control.Exception (SomeException)
import Control.Monad (when)
import UnliftIO (catch)
import Text.Printf (printf)
import Filehub.Layout (Layout(..))
import Filehub.Mime (isMime)
import Filehub.Target qualified as Target
import Filehub.Target.Types.TargetView (TargetView(..))
import Filehub.Types
    ( FilehubEvent (..))
import Filehub.Env qualified as Env
import Filehub.Error ( withServerError, FilehubError(..), FilehubError(..), withServerError )
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
import Filehub.Options (Options(..), parseOptions, TargetOption (..))
import Filehub.Routes qualified as Routes
import Filehub.Server.Context.ReadOnly qualified as Server.Context.ReadOnly
import Filehub.Server.Context.Resolution qualified as Server.Context.Resolution
import Filehub.Server.Context.Session qualified as Server.Context.Session
import Filehub.Server.Middleware qualified as Server.Middleware
import Filehub.Server.Middleware.Display qualified as Server.Middleware.Display
import Filehub.Server.Middleware.Session qualified as Server.Middleware.Session
import Filehub.Server.Desktop qualified as Server.Desktop
import Filehub.Server.Mobile qualified as Server.Mobile
import Filehub.Server.Internal (withQueryParam, clear, copy, paste)
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
import Conduit (ConduitT, ResourceT)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (withArgs)
import UnliftIO (hFlush, stdout)
import Network.Mime qualified as Mime


#ifdef DEBUG
import Effectful ( MonadIO (liftIO) )
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import System.FilePath ((</>))
import Paths_filehub qualified
import System.Directory (makeAbsolute)
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
  , index = \sessionId -> do
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


  , cd = \sessionId mClientPath -> do
      clientPath <- withQueryParam mClientPath
      withServerError do
        root <- Env.getRoot sessionId
        storage <- getStorage sessionId
        storage.cd (ClientPath.fromClientPath root clientPath)
      view sessionId <&> addHeader DirChanged


  , newFile = \sessionId _ (NewFile path) -> do
      withServerError do
        storage <- getStorage sessionId
        storage.new (Text.unpack path)
      view sessionId


  , updateFile = \sessionId _ (UpdatedFile clientPath content) -> do
      let path = clientPath.unClientPath
      withServerError do
        storage <- getStorage sessionId
        storage.write path (Text.encodeUtf8 content ^. lazy)
      view sessionId


  , deleteFile = \sessionId _ mClientPath deleteSelected -> do
      withServerError do
        root <- Env.getRoot sessionId
        storage <- getStorage sessionId

        when (isJust mClientPath) do
          clientPath <- withQueryParam mClientPath
          let p = ClientPath.fromClientPath root clientPath
          storage.delete p

        when deleteSelected do
          allSelecteds <- Selected.allSelecteds sessionId
          forM_ allSelecteds $ \(target, selected) -> do
            Target.withTarget sessionId (Target.getTargetId target) do
              case selected of
                NoSelection -> pure ()
                Selected x xs -> do
                  forM_ (fmap (ClientPath.fromClientPath root) (x:xs)) $ \path -> do
                    storage.delete path
      clear sessionId
      count <- Selected.countSelected sessionId & withServerError
      addHeader count <$> index sessionId


  , newFolder = \sessionId _ (NewFolder path) -> do
      withServerError do
        storage <- getStorage sessionId
        storage.newFolder (Text.unpack path)
      view sessionId


  , newFileModal = \_ _ _ -> pure Template.Desktop.newFileModal


  , newFolderModal = \_ _ _ -> pure Template.Desktop.newFolderModal


  , fileDetailModal = Server.Desktop.fileDetailModal


  , editorModal = \sessionId mClientPath -> do
      display <- Env.getDisplay sessionId & withServerError
      case display of
        Mobile -> Server.Mobile.editorModal sessionId mClientPath
        Desktop -> Server.Desktop.editorModal sessionId mClientPath
        NoDisplay -> undefined


  , search = \sessionId searchWord -> withServerError do
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


  , sortTable = \sessionId order -> do
      Env.setSortFileBy sessionId (fromMaybe ByNameUp order)
      addHeader TableSorted <$> view sessionId


  , selectLayout = \sessionId layout -> do
      Env.setLayout sessionId (fromMaybe ThumbnailLayout layout)
      addHeader LayoutChanged <$> index sessionId


  , selectRows = \sessionId selected -> do
      case selected of
        NoSelection -> do
          logAttention_ [i|No selection: #{sessionId}|]
          throwError InvalidSelection & withServerError
        _ -> do
          Selected.setSelected sessionId selected
          count <- Selected.countSelected sessionId & withServerError
          addHeader count <$> controlPanel sessionId


  , upload = \sessionId _ multipart -> do
      withServerError do
        storage <- getStorage sessionId
        storage.upload multipart
      index sessionId


  , download = \sessionId mClientPath -> do
      clientPath@(ClientPath path) <- withQueryParam mClientPath
      bs <- withServerError do
        storage <- getStorage sessionId
        storage.download clientPath
      pure $ addHeader (printf "attachement; filename=%s" (takeFileName path)) bs


  , copy = \sessionId _ -> do
      copy sessionId
      controlPanel sessionId


  , paste = \sessionId _ -> do
      paste sessionId
      clear sessionId
      count <- Selected.countSelected sessionId & withServerError
      addHeader count <$> index sessionId


  , cancel = \sessionId -> do
      clear sessionId
      count <- Selected.countSelected sessionId & withServerError
      addHeader count <$> index sessionId


  , contextMenu = Server.Desktop.contextMenu


  , initViewer = \sessionId mClientPath -> do
      withServerError do
        clientPath <- withQueryParam mClientPath
        root <- Env.getRoot sessionId
        payload <- Viewer.initViewer sessionId root clientPath
        pure $ addHeader payload NoContent


  , changeTarget = \sessionId mTargetId -> do
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
      readFile $
        case theme of
          Dark -> dir </> "theme-dark.css"
          Light -> dir </> "theme-light.css"
#else
      theme <- Env.getSessionTheme sessionId & withServerError
      pure . LBS.fromStrict $
        case theme of
          Dark -> fromMaybe "no-theme" $ Map.lookup "theme-dark.css" staticFiles
          Light -> fromMaybe "no-theme" $ Map.lookup "theme-light.css" staticFiles
#endif


  , toggleTheme = \sessionId -> do
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


  , favicon = pure $ LBS.fromStrict $(FileEmbed.embedFile "data/filehub/favicon.ico")


  , static = \paths -> do
#ifdef DEBUG
      dir <- liftIO $ Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data/filehub")
      let path = dir </> List.intercalate "/" paths
      let mimetype = Mime.defaultMimeLookup (Text.pack path)
      content <- readFile path
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
            $ LBS.fromStrict
            $ content
        Nothing -> throwError (err404 { errBody = [i|File doesn't exist|]})
#endif


  , offline = pure Template.offline


  , healthz = pure "ok"
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
  state <- ControlPanel.getControlPanelState sessionId
  pure $
    case display of
      Desktop -> Template.Desktop.controlPanel layout theme readOnly state
      Mobile -> Template.Mobile.controlPanel theme readOnly state
      NoDisplay -> Template.Mobile.controlPanel theme readOnly state


serve :: SessionId -> Maybe ClientPath -> Filehub (Headers '[ Header "Content-Type" String
                                                            , Header "Content-Disposition" String
                                                            ] (ConduitT () ByteString (ResourceT IO) ()))
serve sessionId mFile = do
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


-- | Create thumbnailed version of image, pdf, and video.
--   If the image is big we will create a thumbnail by resizing the image, then serve the thumbnail instead.
thumbnail :: SessionId -> Maybe ClientPath -> Filehub (Headers '[ Header "Content-Type" String
                                                                , Header "Content-Disposition" String
                                                                ] LBS.ByteString)
thumbnail sessionId mFile = do
  withServerError do
    storage <- getStorage sessionId
    root <- Env.getRoot sessionId
    clientPath <- withQueryParam mFile
    let path = ClientPath.fromClientPath root clientPath
    file <- storage.get path
    content <-
      case file.size of
        Just size
          | size > 1024 * 1024 -> createThumbnail storage file
        _ -> serveOriginal storage file
    pure
      $ addHeader (ByteString.unpack file.mimetype)
      $ addHeader (printf "inline; filename=%s" (takeFileName path))
      $ content
  where
    serveOriginal storage file =
      if
        | file.mimetype `isMime` "image" ->
          storage.read file
        | otherwise -> throwError InvalidMimeTypeForThumbnail & withServerError

    createThumbnail storage file =
      if
        | file.mimetype `isMime` "image" -> do
           bytes <- storage.read file
           case Picture.decodeImage (LBS.toStrict bytes) of
             Left _ -> pure bytes
             Right image ->
               case resizeToFit 140 140 image of
                 Just resizedImage ->
                     if
                        | file.mimetype `isMime` "image/png" ->
                          either (\_ -> throwError FailedToDecodeImage) pure $ Picture.encodeDynamicPng resizedImage
                        | file.mimetype `isMime` "image/jpeg" ->
                            case resizedImage of
                              Picture.ImageYCbCr8 img -> pure $ Picture.encodeJpeg img
                              _ -> throwError FailedToDecodeImage
                        | file.mimetype `isMime` "image/bmp" ->
                          either (\_ -> throwError FailedToDecodeImage) pure $ Picture.encodeDynamicBitmap resizedImage
                        | file.mimetype `isMime` "image/gif" -> pure bytes
                        | otherwise -> throwError FailedToDecodeImage
                 Nothing -> throwError FailedToDecodeImage
        | otherwise -> throwError InvalidMimeTypeForThumbnail & withServerError


    resizeToFit :: Int -> Int -> Picture.DynamicImage -> Maybe Picture.DynamicImage
    resizeToFit maxHeight maxWidth image = do
      let getResolution img = (Picture.imageWidth img, Picture.imageHeight img)
      let scale :: _ => (Int, Int) -> Picture.Image a -> Picture.Image a
          scale (w, h) x = do
            let s :: Double = min (fromIntegral maxWidth / fromIntegral w) (fromIntegral maxHeight / fromIntegral h)
            let targetW = max 1 (floor $ s * fromIntegral w)
            let targetH = max 1 (floor $ s * fromIntegral h)
            if w <= maxWidth && h <= maxHeight
               then x
               else Picture.STBIR.resize Picture.STBIR.defaultOptions targetW targetH x
      case image of
        Picture.ImageY8 x     -> Just $ Picture.ImageY8 $ scale (getResolution x) x
        Picture.ImageY16 x    -> Just $ Picture.ImageY16 $ scale (getResolution x) x
        Picture.ImageY32 x    -> Just $ Picture.ImageY32 $ scale (getResolution x) x
        Picture.ImageYA8 x    -> Just $ Picture.ImageYA8 $ scale (getResolution x) x
        Picture.ImageYA16 x   -> Just $ Picture.ImageYA16 $ scale (getResolution x) x
        Picture.ImageRGB8 x   -> Just $ Picture.ImageRGB8 $ scale (getResolution x) x
        Picture.ImageRGB16 x  -> Just $ Picture.ImageRGB16 $ scale (getResolution x) x
        Picture.ImageRGBA8 x  -> Just $ Picture.ImageRGBA8 $ scale (getResolution x) x
        Picture.ImageRGBA16 x -> Just $ Picture.ImageRGBA16 $ scale (getResolution x) x
        Picture.ImageYCbCr8 x -> Just $ Picture.ImageYCbCr8 $ scale (getResolution x) x
        Picture.ImageCMYK8 x  -> Just $ Picture.ImageCMYK8 $ scale (getResolution x) x
        Picture.ImageCMYK16 x -> Just $ Picture.ImageCMYK16 $ scale (getResolution x) x
        _ -> Nothing


application :: Env -> Application
application env
  = Server.Middleware.exposeHeaders
  . Server.Middleware.Session.sessionMiddleware env
  . Server.Middleware.dedupHeadersKeepLast
  . Server.Middleware.Display.displayMiddleware env
  . serveWithContextT Routes.api ctx (toServantHandler env)
  $ server
  where

    ctx = Server.Context.Session.sessionHandler env
        :. Server.Context.ReadOnly.readOnlyHandler env
        :. Server.Context.Resolution.desktopOnlyHandler env
        :. Server.Context.Resolution.mobileOnlyHandler env
        :. EmptyContext

------------------------------------
-- main
------------------------------------


main :: IO ()
main = Log.withColoredStdoutLogger \logger -> do
  options <- parseOptions
  sessionPool <- runEff SessionPool.new
  targets <- runEff . runLog "Targets" logger options.verbosity . runFileSystem $ fromTargetOptions options.targets
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
mainDev :: [String] -> IO ()
mainDev args = withArgs args main
