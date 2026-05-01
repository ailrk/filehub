{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the filehub server. Most of the server is implemented with servant handlers.
-- Some features that are hard to implement with servant are provided through wai middleware.
module Filehub.Server (application) where

import Codec.Archive.Zip qualified as Zip
import Conduit (ConduitT, ResourceT, yield, MonadIO (..), MonadUnliftIO (..), runResourceT, runConduit, (.|))
import Conduit qualified
import Control.Applicative (Alternative((<|>)))
import Control.Monad (void, when, join, replicateM)
import Control.Monad.Fix (fix)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (object, KeyValue (..), (.:), withObject, Value)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as Char
import Data.ClientPath (ClientPath (..), AbsPath (..), (<./>), Root (..))
import Data.ClientPath qualified as ClientPath
import Data.ClientPath.IO (validateAbsPath)
import Data.Coerce (coerce)
import Data.File (FileType(..), File(..), FileContent (..), withContent, defaultFileWithContent, FileInfo)
import Data.FileEmbed qualified as FileEmbed
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Function (fix, (&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, catMaybes, isJust, maybeToList)
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i, iii)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID qualified as UUID
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.OIDC (AuthUrl (..), SomeOIDCFlow (..))
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Auth.Simple qualified as Auth.Simple
import Filehub.Auth.Types (AuthId(..))
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env(..))
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly, ConfirmDesktopOnly)
import Filehub.Handler qualified
import Filehub.Locale (Locale (..))
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.QQ qualified
import Filehub.Routes (Api (..))
import Filehub.Routes qualified as Routes
import Filehub.Server.UI.Desktop qualified as Server.Desktop
import Filehub.Server.UI.Mobile qualified as Server.Mobile
import Filehub.Server.Util (withQueryParam, parseHeader')
import Filehub.Session (SessionId(..), TargetView (..))
import Filehub.Session qualified as Session
import Filehub.Session (SessionGet(..))
import Filehub.Session qualified as Session
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Selected qualified as Selected
import Filehub.SharedLink (SharedLinkHash, SharedLinkPermitSet (..), SharedLinkPermit)
import Filehub.Sort qualified as Sort
import Filehub.Template (runTemplate, TemplateContext(..), makeTemplateContext)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Login qualified as Template.Login
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Template.Shared qualified as Template
import Filehub.Theme qualified as Theme
import Filehub.Types (Display (..), LoginForm(..), NewFile(..), NewFolder(..), OpenTarget, Resolution, SearchWord, SortFileBy(..), Theme(..), UIComponent (..), UpdatedFile(..), FilehubEvent (..), RenameFile (..), TargetSessionData (..), MoveFile (..), Resource (..))
import Lens.Micro ((&), (.~), (?~), (<&>))
import Lucid hiding (for_)
import Lucid (Html)
import Network.HTTP.Types.Header (hLocation)
import Network.Mime (MimeType)
import Network.Mime qualified as Mime
import Network.Mime.Extended (isMime)
import Network.URI qualified as URI
import Network.Wai.Middleware.Extended qualified as Wai.Middleware
import Network.Wai.Middleware.Filehub qualified as Wai.Middleware
import Network.Wai.Middleware.Gzip qualified as Wai.Middleware
import Network.Wai.Middleware.RequestLogger qualified as Wai.Middleware (logStdout)
import Prelude hiding (init, readFile)
import Servant (Application , Context (..) , Header , Headers , NoContent (..) , addHeader , err301 , err303 , err400 , err404 , err500 , errHeaders , noHeader , serveWithContextT , errBody, Tagged (..), FromHttpApiData (..))
import Servant (Headers, Header, addHeader)
import Servant.API.EventStream (RecommendedEventSourceHeaders, recommendedEventSourceHeaders)
import Servant.Multipart (MultipartData(..), Mem)
import Servant.Server.Generic (AsServerT)
import System.Directory (removeFile, doesFileExist)
import System.FilePath (takeFileName, (</>), makeRelative, takeDirectory)
import System.IO.Temp qualified as Temp
import System.Random (randomRIO)
import Target.Types (TargetId, AnyTarget)
import Target.Types qualified as Target
import Text.Printf (printf)
import UnliftIO.Exception (SomeException, catch)
import Web.Cookie (SetCookie (..), defaultSetCookie)
import Worker.Task (TaskId, newTaskId)
import UnliftIO (throwIO)
import UnliftIO.STM (readTBQueue, atomically, isEmptyTBQueue, modifyTVar', readTVar, newTVarIO, writeTBQueue)
import Log (logInfo_, logAttention_, logAttention)
import UnliftIO.Async (async, forConcurrently_)
import Control.Monad.Reader (asks)
import Network.Wai (Request(..), responseLBS, responseFile, responseStream)
import Network.HTTP.Types (status404)
import Network.Wai.Application.Static (defaultFileServerSettings, StaticSettings (..), staticApp)
import WaiAppStatic.Types (MaxAge(..), toPiece, LookupResult (..), File(..), unsafeToPiece)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C (CTime(..))
import Data.Binary.Builder qualified as Builder
import Filehub.Server.UI (index, clear, controlPanel, sideBar, view)
import Filehub.Server.UI qualified
import Filehub.Server.Login qualified
import Filehub.Server.File qualified

#ifdef DEBUG
import UnliftIO (MonadIO(liftIO))
import Paths_filehub qualified
import System.Directory (makeAbsolute)
import Data.ByteString (readFile)
import Data.Text.Lazy qualified as LText
#endif


------------------------------------
-- Static files
------------------------------------


-- | Static files are embeded into the final excutable. The key is the path of the file.
--   e.g main.js -> (data/filehub/main.js)
staticFiles :: Map FilePath ByteString
staticFiles = Map.fromList
  $(FileEmbed.embedDir "data/filehub")


------------------------------------
-- Sever endpoints
------------------------------------


server :: Env -> Api (AsServerT Filehub)
server env = Api
  { initialize            = initialize
  , home                  = home
  , refresh               = refresh
  , listen                = listen
  , loginPage             = Filehub.Server.Login.loginPage
  , loginToggleTheme      = Filehub.Server.Login.loginToggleTheme
  , loginChangeLocale     = Filehub.Server.Login.loginChangeLocale
  , loginAuthSimple       = Filehub.Server.Login.loginAuthSimple
  , loginAuthOIDCRedirect = Filehub.Server.Login.loginAuthOIDCRedirect
  , loginAuthOIDCCallback = Filehub.Server.Login.loginAuthOIDCCallback
  , logout                = Filehub.Server.Login.logout
  , cd                    = Filehub.Server.File.cd
  , newFile               = Filehub.Server.File.newFile
  , updateFile            = Filehub.Server.File.updateFile
  , rename                = Filehub.Server.File.rename
  , delete                = Filehub.Server.File.delete
  , newFolder             = Filehub.Server.File.newFolder
  , renameModal           = Filehub.Server.UI.renameModal
  , newFileModal          = Filehub.Server.UI.newFileModal
  , newFolderModal        = Filehub.Server.UI.newFolderModal
  , fileDetailModal       = Filehub.Server.UI.fileDetailModal
  , editorModal           = Filehub.Server.UI.editorModal
  , search                = search
  , sortTable             = Filehub.Server.UI.sortTable
  , selectLayout          = Filehub.Server.UI.selectLayout
  , selectRows            = Filehub.Server.UI.selectRows
  , upload                = Filehub.Server.File.upload
  , download              = Filehub.Server.File.download
  , copy                  = Filehub.Server.File.copy
  , copy1                 = Filehub.Server.File.copy1
  , paste                 = Filehub.Server.File.paste
  , move                  = Filehub.Server.File.move
  , cancel                = cancel
  , contextMenu           = Filehub.Server.UI.contextMenu
  , initViewer            = Filehub.Server.UI.initViewer
  , open                  = Filehub.Server.UI.open
  , changeTarget          = changeTarget
  , shared                = shared
  , sharedAuth            = sharedAuth
  , themeCss              = themeCss
  , toggleTheme           = Filehub.Server.UI.toggleTheme
  , changeLocale          = Filehub.Server.UI.changeLocale
  , serve                 = Filehub.Server.File.serve env
  , toggleSidebar         = Filehub.Server.UI.toggleSidebar
  , thumbnail             = thumbnail
  , manifest              = manifest
  , favicon               = pure $(FileEmbed.embedFile "data/filehub/favicon.ico")
  , static                = static
  , offline               = pure Template.offline
  , healthz               = healthz
#ifdef DEBUG
  , debug1                = \_ -> pure $ addHeader (Dummy "Hello") NoContent
#endif
  }


healthz :: SessionId -> Filehub Text
healthz _ = do
  let gitRev = Text.dropAround Char.isSpace
             $ Text.decodeUtf8 $(Filehub.QQ.getGitRev)
  pure $ Text.concat [ gitRev, ", ok." ]


initialize :: SessionId -> Resolution -> Filehub (Html ())
initialize sessionId res = do
  Session.Pool.update sessionId \s -> s & #resolution ?~ res
  clear sessionId
  index sessionId


-- | The main entrance of the application.
-- Home reloads static resources (js, css, etc).
-- Whenver you need to re-render the index page, call the `index` free function instead,
-- which only render the element #index.
--
-- `bootstrap` is used to query the device resolution before rendering anything. Once
-- the session is bootstrapped, display information will be available for all subsequent
-- requests.
--
-- The frontend js deletes the `display` cookie on `pageunload`, so the backend can
-- start a full reload from the bootstrap stage.
home :: SessionId -> ConfirmLogin -> Filehub (Html ())
home sessionId _  = do
  display <- Session.get sessionId (.display)
  m <- manifest
  let background
        = fromMaybe "#000000"
        . flip parseMaybe m
        . withObject "manifest"
        $ (.: "theme_color")
  clear sessionId
  case display of
    NoDisplay -> pure Template.bootstrap
    -- Index is initially hidden, the frontend will play an intro animation, set
    -- the opacity to 1.
    -- `home` is the only endpoint that needs to play the animation, so it's important
    -- that the animation classes are removed from the js.
    Desktop -> do
      html <- Server.Desktop.index sessionId
      pure $ Template.withDefault display background do
        html `with` [ class_ "hidden fade-in " ]
    Mobile -> do
      html <- Server.Mobile.index sessionId
      pure $ Template.withDefault display background do
        html `with` [ class_ "hidden fade-in" ]


-- | Force to refresh a component. It's useful for the client to selectively update ui.
refresh :: SessionId -> ConfirmLogin -> Maybe UIComponent -> Filehub (Html ())
refresh sessionId _ mUIComponent = do
  case mUIComponent of
    Just UIComponentContronPanel -> do
      controlPanel sessionId
    Just UIComponentSideBar -> do
      sideBar sessionId
    Just UIComponentView -> do
      view sessionId
    Just UIComponentIndex -> do
      index sessionId
    Nothing ->
      throwIO do HTTPError (err400 { errBody = [i|Invalid ui component|]})


-- | Creating a notification conduit. The conduit tries to read notifications
-- from the `notifications :: TBQueue Notification` and handle each message accordingly.
--
-- == Task
-- If a notification has a task id, it associates with a task that has been created earlier.
-- The worker pool reports the progress by sending a notification to this thread. We can
-- then choose yeild it to downtream or swallow it.
-- When a task is completed, a `TaskCompleted` notification will be sent. The session
-- maintains a set of pending task ids, every time we received a `TaskCompleted` message
-- we remove the task Id from the pending task set. We can close the notification if there
-- is no more pending tasks.
--
-- This means the notification conduit is created on demand. That is: it's created only when
-- we have a task running in the back ground. When there are multiple tasks, they share the
-- same conduit; when there are no pending task, the conduit finshes; when there is not task,
-- no conduit.
listen :: SessionId -> ConfirmLogin -> Filehub (RecommendedEventSourceHeaders (ConduitT () Notification IO ()))
listen sessionId _ = recommendedEventSourceHeaders <$> do
  notifications <- Session.get sessionId (.notifications)
  pendingTasks  <- Session.get sessionId (.pendingTasks)
  streamAtomically \loop -> do
    n <- readTBQueue notifications
    case n of
      TaskCompleted taskId _ -> do
        modifyTVar' pendingTasks (Set.delete taskId)
        tasksRemaining <- readTVar pendingTasks
        if Set.null tasksRemaining
           then do
             clearQueue notifications
             pure (yield n)
           else pure do yield n; loop
      DeleteProgressed _ _ _ -> pure do yield n; loop
      PasteProgressed _ _ _  -> pure do yield n; loop
      MoveProgressed _ _ _   -> pure do yield n; loop
      UploadProgressed _ _ _ -> pure do yield n; loop
      Pong                   -> pure do yield n; loop
  where
    streamAtomically action =
      withRunInIO \runInIO -> do
        pure do
          fix \loop -> join . liftIO . runInIO $ atomically do
            action loop

    clearQueue notifications =
      fix \popMore -> do
        empty <- isEmptyTBQueue notifications
        when (not empty) do
          _ <- readTBQueue notifications
          popMore


search :: SessionId -> ConfirmLogin -> SearchWord -> Filehub (Html ())
search sessionId _ searchWord = do
  storage <- Session.get sessionId (.storage)
  display <- Session.get sessionId (.display)
  ctx <- makeTemplateContext sessionId
  files   <- storage.lsCwd
  case display of
    Mobile    -> pure $ runTemplate ctx (Template.search searchWord files Template.Mobile.table)
    Desktop   -> pure $ runTemplate ctx (Template.search searchWord files Template.Desktop.table)
    NoDisplay -> error "impossible"



cancel :: SessionId -> ConfirmLogin -> Filehub (Headers '[Header "X-Filehub-Selected-Count" Int] (Html ()))
cancel sessionId _ = do
  clear sessionId
  count <- length <$> Selected.allSelecteds sessionId
  addHeader count <$> index sessionId


changeTarget :: SessionId -> ConfirmLogin -> Maybe TargetId
             -> Filehub (Headers '[Header "HX-Trigger-After-Swap" FilehubEvent] (Html ()))
changeTarget sessionId _ mTargetId = do
  savedTargetId <- do
    TargetView saved _ <- Session.get sessionId (.currentTarget)
    pure $ Target.getTargetId saved

  let restore = Session.set sessionId (.currentTarget) savedTargetId
  targetId <- withQueryParam mTargetId
  Session.set sessionId (.currentTarget) targetId

  html <- withRunInIO \unlift -> do
    unlift (index sessionId)
      `catch` \(e :: SomeException) -> unlift do
        restore
        logAttention "[5ngtzx] Change target failed" (show e)
        throwIO (HTTPError (err500 { errBody = [i|Invalid target|]}))

  pure $ addHeader TargetChanged html


-- TODO
-- client has no permit -> auth
-- client as permit
--      session has no permit -> auth
--      session has permit ->
--        sessiono permt != clinet permit -> auth
--        sessiono permt == clinet permit ->
--          link hash is not in permit set -> auth
--          link hash is in permit set ->
--             has client path ->
--                client path is folder index -> render folder
--                client path is file         -> serve file
--             no client path  -> render shared index
--
-- Shared link also have a session?
-- So can we reuse the session?
-- Normally session rely on a target
--
-- but shared link doesn't map to a target
-- so should we create a synthesized target that forwards the link?
shared :: SessionId -> Maybe SharedLinkPermit -> SharedLinkHash -> Maybe ClientPath -> Filehub (Html ())
shared sessionId mClientPermit hash mClientPath = do
  case mClientPermit of
    Just clientPermit -> do
      sharedLinkPermit <- Session.get sessionId (.sharedLinkPermit)
      case sharedLinkPermit of
        Just (SharedLinkPermitSet permit hashes)
          | clientPermit /= permit         -> goAuth
          | hash `Set.member` hashes       -> goAuth
          | Just clientPath <- mClientPath -> do
              undefined
              -- TODO
              -- convert shared client path to normal storage path from SharedLink
              -- check if file exist
              -- check if file is a directory
              -- render index/serve file accordingly
          | otherwise -> sharedIndex
        Nothing -> goAuth
    Nothing -> goAuth
  where
    goAuth = throwIO do
      HTTPError err303 { errHeaders = [( "Location" , "/s/auth")] }

    sharedIndex = do
      pure mempty


sharedAuth :: SessionId -> Filehub (Html ())
sharedAuth sessionId = undefined


themeCss :: SessionId -> Filehub ByteString
themeCss sessionId = do
  theme <- Session.get sessionId (.theme)
  customThemeDark  <- (fmap . fmap) Theme.customTheme2Css (asks (.customThemeDark))
  customThemeLight <- (fmap . fmap) Theme.customTheme2Css (asks (.customThemeLight))
#ifdef DEBUG
  dir <- liftIO $ Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data/filehub")
  case theme of
    Dark  -> liftIO $ maybe (readFile (dir </> "theme-dark.css")) pure customThemeDark
    Light -> liftIO $ maybe (readFile (dir </> "theme-light.css")) pure customThemeLight
#else
  pure
    case theme of
      Dark  -> fromMaybe "no-theme" $ customThemeDark <|> Map.lookup "theme-dark.css" staticFiles
      Light -> fromMaybe "no-theme" $ customThemeLight <|> Map.lookup "theme-light.css" staticFiles
#endif



thumbnail :: SessionId -> ConfirmLogin -> Maybe ClientPath
          -> Filehub (Headers '[ Header "Content-Type" String
                               , Header "Content-Disposition" String
                               , Header "Cache-Control" String
                               ]
                               (ConduitT () ByteString (ResourceT IO) ()))
thumbnail sessionId _ mFile = do
  root       <- Session.get sessionId (.root)
  storage    <- Session.get sessionId (.storage)
  clientPath <- withQueryParam mFile
  let path   =  ClientPath.fromClientPath root clientPath
  file       <- storage.get path
  conduit    <- serveOriginal storage file

  pure
    . addHeader (ByteString.unpack file.mimetype)
    . addHeader (printf "inline; filename=%s" (coerce takeFileName path :: String))
    . addHeader "public, max-age=31536000, immutable"
    $ conduit

  where
    serveOriginal storage file =
      if
        | file.mimetype `isMime` "image" -> storage.readStream file Nothing Nothing
        | otherwise                      -> throwIO (FilehubError FormatError "Invalid mime type for thumbnail")



-- It's for PWA. More on https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/Manifest
manifest :: Filehub Value
manifest = do
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


-- The production implementation uses static files embeded in the executable, while the
-- debug implementation uses path so you can hot reload frontend code.
--
-- == Content-Type header is used to specify the content type. Note it's hard to set the
-- content type dynamically with servant because the type makes an assumption of the content
-- type. This handler simply adds the correct Content-Type, and the `dedupHeadersKeepLast`
-- middleware will strip the default servant header.
--
-- == Cache-Control header is required to make sure static files are properly cached.
static :: [FilePath] -> Filehub (Headers '[ Header "Content-Type" String
                                          , Header "Cache-Control" String
                                          , Header "ETag" String ] ByteString)
static paths = do
#ifdef DEBUG
  dir          <- liftIO $ Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data/filehub")
  let path     =  dir </> List.intercalate "/" paths
  let mimetype =  Mime.defaultMimeLookup (Text.pack path)
  content      <- liftIO . readFile $ path

#else
  let path = List.intercalate "/" paths
  content <- case Map.lookup path staticFiles of
    Just c -> pure c
    Nothing -> throwIO do HTTPError (err404 { errBody = [i|File doesn't exist|]})
#endif

  let mimetype = Mime.defaultMimeLookup (Text.pack path)
  let etag    = "\"" <> ByteString.unpack (Base64.encode (SHA256.hash content)) <> "\""
  pure
    . addHeader (ByteString.unpack mimetype)
    . addHeader "public, no-cache"
    . addHeader etag
    $ content


------------------------------------
-- application
------------------------------------


application :: Env -> Application
application env
  = Wai.Middleware.gzip Wai.Middleware.defaultGzipSettings
  . (if env.enableWAILog then Wai.Middleware.logStdout else id)
  . Wai.Middleware.stripCookiesForStatic
  . Wai.Middleware.exposeHeaders
  . Wai.Middleware.sessionMiddleware env
  . Wai.Middleware.dedupHeadersKeepLast
  . Wai.Middleware.displayMiddleware env
  . serveWithContextT Routes.api ctx (Filehub.Handler.toServantHandler env)
  $ server env
  where
    ctx = Filehub.Handler.sessionHandler env
        :. Filehub.Handler.readOnlyHandler env
        :. Filehub.Handler.desktopOnlyHandler env
        :. Filehub.Handler.mobileOnlyHandler env
        :. Filehub.Handler.loginHandler env
        :. Filehub.Handler.sharedLinkPermitHandler env
        :. EmptyContext
