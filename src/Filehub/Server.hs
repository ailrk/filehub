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

import Conduit (ConduitT, ResourceT, yield)
import Control.Applicative (Alternative((<|>)))
import Control.Monad (when, join)
import Data.Aeson (object, KeyValue (..), (.:), withObject, Value)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.ClientPath (ClientPath (..))
import Data.ClientPath qualified as ClientPath
import Data.File (File(..), defaultFileWithContent, FileContent (..))
import Data.FileEmbed qualified as FileEmbed
import Data.Function (fix, (&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String.Interpolate (i, iii)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Effectful ( withRunInIO )
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logInfo_, logAttention_)
import Effectful.Reader.Dynamic (asks)
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.OIDC (AuthUrl (..), SomeOIDCFlow (..))
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Auth.Simple qualified as Auth.Simple
import Filehub.Cookie qualified as Cookie
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env(..))
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly, ConfirmDesktopOnly)
import Filehub.Handler qualified
import Filehub.Locale (Locale (..))
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Routes (Api (..))
import Filehub.Routes qualified as Routes
import Filehub.Server.Component (index, view, controlPanel, sideBar, toolBar)
import Filehub.Server.Component.Desktop qualified as Server.Desktop
import Filehub.Server.Component.Mobile qualified as Server.Mobile
import Filehub.Server.Delete (delete)
import Filehub.Server.Download (download)
import Filehub.Server.InitViewer (initViewer)
import Filehub.Server.Internal (withQueryParam, parseHeader')
import Filehub.Server.Internal qualified as Server.Internal
import Filehub.Server.Move (move)
import Filehub.Server.Paste (paste)
import Filehub.Server.Upload (upload)
import Filehub.Session (SessionId(..), TargetView (..))
import Filehub.Session qualified as Session
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Selected qualified as Selected
import Filehub.Template (runTemplate, TemplateContext(..), makeTemplateContext)
import Filehub.Template.Shared qualified as Template
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Login qualified as Template.Login
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Theme qualified as Theme
import Filehub.Types
  ( ControlPanelState (..)
  , Display (..)
  , Layout (..)
  , LoginForm(..)
  , NewFile(..)
  , NewFolder(..)
  , OpenTarget
  , Resolution
  , SearchWord
  , Selected (..)
  , SortFileBy(..)
  , Theme(..)
  , UIComponent (..)
  , UpdatedFile(..)
  , UpdatedFile(..)
  , FilehubEvent (..))
import Lens.Micro ((.~), (?~))
import Lucid
import Network.HTTP.Types.Header (hLocation)
import Network.Mime qualified as Mime
import Network.Mime.Extended (isMime)
import Network.URI qualified as URI
import Network.Wai.Middleware.Extended qualified as Wai.Middleware
import Network.Wai.Middleware.Filehub qualified as Wai.Middleware
import Network.Wai.Middleware.Gzip qualified as Wai.Middleware
import Network.Wai.Middleware.RequestLogger qualified as Wai.Middleware (logStdout)
import Prelude hiding (init, readFile)
import Servant
  ( Application
  , Context (..)
  , Header
  , Headers
  , NoContent (..)
  , addHeader
  , err301
  , err303
  , err400
  , err404
  , err500
  , errHeaders
  , noHeader
  , serveWithContextT
  , errBody
  )
import Servant.API.EventStream (RecommendedEventSourceHeaders, recommendedEventSourceHeaders)
import Servant.Server.Generic (AsServerT)
import System.FilePath (takeFileName)
import Target.Types (TargetId)
import Target.Types qualified as Target
import Text.Printf (printf)
import UnliftIO.Exception (SomeException, catch)
import UnliftIO.STM (readTBQueue, atomically, modifyTVar', readTVar, isEmptyTBQueue)
import Web.Cookie (SetCookie (..), defaultSetCookie)
import Filehub.Auth.Types (AuthId(..))
import Data.UUID qualified as UUID
import Filehub.SharedLink (SharedLinkHash, SharedLinkPermitSet (..), SharedLinkPermit)
import Debug.Trace (traceShowM)
import Data.Char qualified as Char
import Filehub.QQ qualified


#ifdef DEBUG
import Effectful ( MonadIO (liftIO) )
import System.FilePath ((</>))
import Data.Functor ((<&>))
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


server :: Api (AsServerT Filehub)
server = Api
  { initialize            = initialize
  , home                  = home
  , refresh               = refresh
  , listen                = listen
  , loginPage             = loginPage
  , loginToggleTheme      = loginToggleTheme
  , loginChangeLocale     = loginChangeLocale
  , loginAuthSimple       = loginAuthSimple
  , loginAuthOIDCRedirect = loginAuthOIDCRedirect
  , loginAuthOIDCCallback = loginAuthOIDCCallback
  , logout                = logout
  , cd                    = cd
  , newFile               = newFile
  , updateFile            = updateFile
  , delete                = delete
  , newFolder             = newFolder
  , newFileModal          = newFileModal
  , newFolderModal        = newFolderModal
  , fileDetailModal       = fileDetailModal
  , editorModal           = editorModal
  , search                = search
  , sortTable             = sortTable
  , selectLayout          = selectLayout
  , selectRows            = selectRows
  , upload                = upload
  , download              = download
  , copy                  = copy
  , copy1                 = copy1
  , paste                 = paste
  , move                  = move
  , cancel                = cancel
  , contextMenu           = contextMenu
  , initViewer            = initViewer
  , open                  = open
  , changeTarget          = changeTarget
  , shared                = shared
  , sharedAuth            = sharedAuth
  , themeCss              = themeCss
  , toggleTheme           = toggleTheme
  , changeLocale          = changeLocale
  , serve                 = serve
  , thumbnail             = thumbnail
  , manifest              = manifest
  , favicon               = pure $(FileEmbed.embedFile "data/filehub/favicon.ico")
  , static                = static
  , offline               = pure Template.offline
  , healthz               = healthz
#ifdef DEBUG
  , debug1                = \_ -> pure $ addHeader (Dummy "Hello") NoContent
  , preview               = preview
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
  Server.Internal.clear sessionId
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
  display <- Session.getDisplay sessionId
  m <- manifest
  let background
        = fromMaybe "#000000"
        . flip parseMaybe m
        . withObject "manifest"
        $ (.: "theme_color")
  Server.Internal.clear sessionId
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
      throwError do HTTPError (err400 { errBody = [i|Invalid ui component|]})


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
-- same conduit; when there are no pending task, the conduit finshes; when tehre is not task,
-- no conduit.
listen :: SessionId -> ConfirmLogin -> Filehub (RecommendedEventSourceHeaders (ConduitT () Notification IO ()))
listen sessionId _ = recommendedEventSourceHeaders <$> do
  notifications <- Session.getSessionNotifications sessionId
  pendingTasks  <- Session.getPendingTasks sessionId
  pure $ fix \loop -> join $ atomically do
    notification <- readTBQueue notifications
    case notification of
      TaskCompleted taskId -> do
        modifyTVar' pendingTasks (Set.delete taskId)
        tasksRemaining <- readTVar pendingTasks
        if Set.null tasksRemaining
           then do -- clear notification
             fix \popMore -> do
               empty <- isEmptyTBQueue notifications
               when (not empty) do
                _ <- readTBQueue notifications
                popMore
             pure do
              yield notification
           else
           pure do
             yield notification
             loop
      DeleteProgressed _ _ -> pure do yield notification >> loop
      PasteProgressed _ _  -> pure do yield notification >> loop
      MoveProgressed _ _   -> pure do yield notification >> loop
      UploadProgressed _ _ -> pure do yield notification >> loop
      Pong                 -> pure do yield notification >> loop


-- | Return the login page
loginPage :: SessionId -> Maybe Text -> Maybe Text -> Filehub (Html ())
loginPage sessionId cookie Nothing = do
  ctx@TemplateContext { noLogin } <- makeTemplateContext sessionId
  if noLogin
     then go
     else do
       case fmap Text.encodeUtf8 cookie >>= parseHeader' >>= Cookies.fromCookies of
         Just authId' -> do
           authId <- Session.getAuthId sessionId
           if authId == Just authId'
              then go
              else pure $ runTemplate ctx Template.Login.login
         Nothing -> pure $ runTemplate ctx Template.Login.login
  where
    go = throwError do HTTPError (err301 { errHeaders = [(hLocation, "/")] })
loginPage sessionId _ (Just _) = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Login.login


loginToggleTheme :: SessionId -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
loginToggleTheme sessionId = do
  theme <- Session.getSessionTheme sessionId
  case theme of
    Theme.Light -> Session.setSessionTheme sessionId Theme.Dark
    Theme.Dark  -> Session.setSessionTheme sessionId Theme.Light
  ctx <- makeTemplateContext sessionId
  let html = runTemplate ctx Template.Login.login'
  pure $ addHeader ThemeChanged html


loginChangeLocale :: SessionId -> Maybe Locale -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
loginChangeLocale _ Nothing = throwError (FilehubError LocaleError "Invalid locale")
loginChangeLocale sessionId (Just locale) = do
  Session.setSessionLocale sessionId locale
  ctx <- makeTemplateContext sessionId
  let html = runTemplate ctx Template.Login.login'
  pure $ addHeader LocaleChanged html


-- | Handle the simple authetication login.
loginAuthSimple :: SessionId -> LoginForm
                -> Filehub (Headers '[ Header "Set-Cookie" SetCookie
                                     , Header "HX-Redirect" Text
                                     ] (Html ()))
loginAuthSimple sessionId form@(LoginForm username _) =  do
  ctx <- makeTemplateContext sessionId
  let failed = runTemplate ctx (Template.Login.loginFailed Nothing)
  mSession <- Auth.Simple.authenticateSession sessionId form
  case mSession of
    Just session -> do
      case session.authId of
        Just (AuthId authId) -> do
          let bytes = UUID.toASCIIBytes authId
          let setCookie = defaultSetCookie
                { setCookieName     = "authId"
                , setCookieValue    = bytes
                , setCookieExpires  = Just session.expireDate
                , setCookieHttpOnly = True
                , setCookiePath     = Just "/"
                , setCookieSecure   = True
                }
          logInfo_ [i|[2445sz] User #{username} logged in|]
          addHeader setCookie . addHeader "/" <$> pure mempty
        Nothing -> noHeader . noHeader <$> (pure failed)

    Nothing -> do
      noHeader . noHeader <$> (pure failed)


loginAuthOIDCRedirect :: SessionId -> Text -> Filehub NoContent
loginAuthOIDCRedirect sessionId providerName = do
  stage <- Auth.OIDC.initialize providerName >>= Auth.OIDC.authorize
  Auth.OIDC.setSessionOIDCFlow sessionId (Just stage)
  case stage of
    Auth.OIDC.AuthRequestPrepared _ _ _ _ (AuthUrl url) ->
      throwError do
        HTTPError err303
          { errHeaders =
              [( "Location"
               , ByteString.pack (URI.uriToString id url "")
               )]
          }


loginAuthOIDCCallback :: SessionId
                      -> Maybe Text
                      -> Maybe Text
                      -> Maybe Text
                      -> Maybe Text
                      -> Maybe Text
                      -> Maybe Text
                      -> Filehub NoContent
loginAuthOIDCCallback sessionId (Just code) (Just state) _ _ _ _ = do
  Auth.OIDC.getSessionOIDCFlow sessionId >>= \case
    Just (SomeOIDCFlow (stage@Auth.OIDC.AuthRequestPrepared {})) -> do
        Auth.OIDC.callback stage code state
          >>= Auth.OIDC.exchangeToken
          >>= Auth.OIDC.verifyToken
          >>= Auth.OIDC.authenticateSession sessionId
          >>= Auth.OIDC.setSessionOIDCFlow sessionId . Just
    _ -> do
      logAttention_ "[s9vf9d] OIDC Error: invalid stage"
      pure ()
  session <- Session.Pool.get sessionId
  case session.authId of
    Just (AuthId authId) -> do
      let bytes = UUID.toASCIIBytes authId
      let setCookie = defaultSetCookie
            { setCookieName     = "authId"
            , setCookieValue    = bytes
            , setCookieExpires  = Just session.expireDate
            , setCookieHttpOnly = True
            , setCookiePath     = Just "/"
            , setCookieSecure   = True
            }
      throwError do
        HTTPError err303
          { errHeaders = [( "Location" , "/"), ("Set-Cookie", Cookies.renderSetCookie setCookie)]
          }
    Nothing -> do
      throwError do
        HTTPError err303
          { errHeaders = [( "Location" , "/login")]
          }


loginAuthOIDCCallback _ _ _ mErr mErrDescription _ _ = do
  let message = fromMaybe "" mErr <> ", " <> fromMaybe "" mErrDescription
  throwError do
    HTTPError err303
      { errHeaders = [( "Location" , "/login?error=\"" <> Text.encodeUtf8 message <> "\"" )]
      }


logout :: SessionId -> ConfirmLogin -> Filehub (Headers '[ Header "Set-Cookie" SetCookie
                                                         , Header "HX-Redirect" Text
                                                         ] NoContent)
logout sessionId _ = do
  session <- Session.Pool.get sessionId
  let mSetCookie =
        fmap (\(AuthId authId) -> do
          let bytes = UUID.toASCIIBytes authId
          defaultSetCookie
            { setCookieName     = "authId"
            , setCookieValue    = bytes
            , setCookieExpires  = Just session.expireDate
            , setCookieHttpOnly = True
            , setCookiePath     = Just "/"
            , setCookieSecure   = True
            })
       session.authId

  case (,) <$> mSetCookie  <*> session.authId of
    Just (setCookie, authId) -> do
      Session.setAuthId sessionId Nothing
      Auth.OIDC.setSessionOIDCFlow sessionId Nothing
      ActiveUser.Pool.delete authId
      addHeader
        (setCookie
          { setCookieExpires = Just (UTCTime (fromGregorian 1970 1 1) 0) })
        . addHeader "/login"
        <$> pure NoContent
    Nothing -> noHeader . noHeader <$> pure NoContent


cd :: SessionId -> ConfirmLogin -> Maybe ClientPath
   ->  Filehub (Headers '[ Header "HX-Trigger-After-Swap" FilehubEvent ] (Html ()))
cd sessionId _ mClientPath = do
  clientPath <- withQueryParam mClientPath
  root    <- Session.getRoot sessionId
  storage <- Session.getStorage sessionId
  storage.cd (ClientPath.fromClientPath root clientPath)
  html <- do
    toolBar' <- toolBar sessionId
    view'    <- view sessionId
    pure do
      toolBar' `with` [ term "hx-swap-oob" "true" ]
      view'
  pure $ addHeader DirChanged html


newFile :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFile -> Filehub (Html ())
newFile sessionId _ _ (NewFile path) = do
  storage <- Session.getStorage sessionId
  storage.new (Text.unpack path)
  view sessionId


updateFile :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> UpdatedFile -> Filehub (Html ())
updateFile sessionId _ _ (UpdatedFile clientPath content) = do
  let path = clientPath.unClientPath
  storage <- Session.getStorage sessionId
  storage.write path $ defaultFileWithContent
    { path     = path
    , content  = FileContentRaw (Text.encodeUtf8 content)
    }
  view sessionId


newFolder :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFolder -> Filehub (Html ())
newFolder sessionId _ _ (NewFolder path) = do
  storage <- Session.getStorage sessionId
  storage.newFolder (Text.unpack path)
  view sessionId


copy :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Filehub (Html ())
copy sessionId _ _ = do
  Copy.select sessionId
  Copy.copy sessionId
  controlPanel sessionId


copy1 :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Maybe ClientPath -> Filehub (Html ())
copy1 sessionId _ _ mClientPath = do
  clientPath <- withQueryParam mClientPath
  Server.Internal.clear sessionId
  Selected.setSelected sessionId (Selected clientPath [])
  Copy.select sessionId
  Copy.copy sessionId
  index sessionId


newFileModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Filehub (Html ())
newFileModal sessionId _ _ _ = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.newFileModal


newFolderModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Filehub (Html ())
newFolderModal sessionId  _ _ _ = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.newFolderModal


fileDetailModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> Maybe ClientPath -> Filehub (Html ())
fileDetailModal sessionId _ _ mPath = do
  Server.Desktop.fileDetailModal sessionId mPath


editorModal :: SessionId -> ConfirmLogin -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId _ mClientPath = do
  display <- Session.getDisplay sessionId
  case display of
    Mobile    -> Server.Mobile.editorModal sessionId mClientPath
    Desktop   -> Server.Desktop.editorModal sessionId mClientPath
    NoDisplay -> error "impossible"


selectLayout :: SessionId -> ConfirmLogin -> Maybe Layout -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
selectLayout sessionId _ layout = do
  Session.setLayout sessionId (fromMaybe ThumbnailLayout layout)
  addHeader LayoutChanged <$> index sessionId


sortTable :: SessionId ->  ConfirmLogin -> Maybe SortFileBy -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
sortTable sessionId _ order = do
  display <- Session.getDisplay sessionId
  Session.setSortFileBy sessionId (fromMaybe ByNameUp order)
  html <- do
    view' <- view sessionId
    case display of
      Mobile -> do
        toolBar' <- Server.Mobile.toolBar sessionId
        pure do
          toolBar' `with` [ term "hx-swap-oob" "true" ]
          view'
      Desktop   -> pure view'
      NoDisplay -> pure view'
  pure $ addHeader TableSorted html


search :: SessionId -> ConfirmLogin -> SearchWord -> Filehub (Html ())
search sessionId _ searchWord = do
  ctx <- makeTemplateContext sessionId
  display <- Session.getDisplay sessionId
  storage <- Session.getStorage sessionId
  files   <- storage.lsCwd
  case display of
    Mobile    -> pure $ runTemplate ctx (Template.search searchWord files Template.Mobile.table)
    Desktop   -> pure $ runTemplate ctx (Template.search searchWord files Template.Desktop.table)
    NoDisplay -> error "impossible"


selectRows :: SessionId -> ConfirmLogin -> Selected -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
selectRows sessionId _ selected = do
  case selected of
    NoSelection -> do
      Selected.setSelected sessionId NoSelection
      sideBar'      <- sideBar sessionId
      controlPanel' <- controlPanel sessionId
      pure $ addHeader 0 do
        sideBar' `with` [ term "hx-swap-oob" "true" ]
        controlPanel'
    _ -> do
      Selected.setSelected sessionId selected
      count         <- length <$> Selected.allSelecteds sessionId
      sideBar'      <- sideBar sessionId
      controlPanel' <- controlPanel sessionId
      pure $ addHeader count do
        sideBar' `with` [ term "hx-swap-oob" "true" ]
        controlPanel'


contextMenu :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> [ClientPath] -> Filehub (Html ())
contextMenu sessionId _ _ paths = Server.Desktop.contextMenu sessionId paths


cancel :: SessionId -> ConfirmLogin -> Filehub (Headers '[Header "X-Filehub-Selected-Count" Int] (Html ()))
cancel sessionId _ = do
  Server.Internal.clear sessionId
  count <- length <$> Selected.allSelecteds sessionId
  addHeader count <$> index sessionId


open :: SessionId -> ConfirmLogin -> Maybe OpenTarget -> Maybe ClientPath
     -> Filehub (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
open _ _ mTarget mClientPath = do
  clientPath <- withQueryParam mClientPath
  target     <- withQueryParam mTarget
  pure $ addHeader (Opened target clientPath) NoContent


changeTarget :: SessionId -> ConfirmLogin -> Maybe TargetId
             -> Filehub (Headers '[Header "HX-Trigger-After-Swap" FilehubEvent] (Html ()))
changeTarget sessionId _ mTargetId = do
  savedTargetId <- do
    TargetView saved _ <- Session.currentTarget sessionId
    pure $ Target.getTargetId saved

  let restore = Session.changeCurrentTarget sessionId savedTargetId
  targetId <- withQueryParam mTargetId
  Session.changeCurrentTarget sessionId targetId

  html <- withRunInIO \unlift -> do
    unlift (index sessionId)
      `catch` \(_ :: SomeException) -> unlift do
        restore
        throwError (HTTPError (err500 { errBody = [i|Invalid target|]}))

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
      mSharedLinkPermit <- Session.getSessionSharedLinkPermit sessionId
      case mSharedLinkPermit of
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
    goAuth = throwError do
      HTTPError err303 { errHeaders = [( "Location" , "/s/auth")] }

    sharedIndex = do
      pure mempty


sharedAuth :: SessionId -> Filehub (Html ())
sharedAuth sessionId = undefined


themeCss :: SessionId -> Filehub ByteString
themeCss sessionId = do
  customThemeDark  <- (fmap . fmap) Theme.customTheme2Css (asks @Env (.customThemeDark))
  customThemeLight <- (fmap . fmap) Theme.customTheme2Css (asks @Env (.customThemeLight))
#ifdef DEBUG
  theme <- Session.getSessionTheme sessionId
  dir <- liftIO $ Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data/filehub")
  case theme of
    Dark  -> liftIO $ maybe (readFile (dir </> "theme-dark.css")) pure customThemeDark
    Light -> liftIO $ maybe (readFile (dir </> "theme-light.css")) pure customThemeLight
#else
  theme <- Session.getSessionTheme sessionId
  pure
    case theme of
      Dark  -> fromMaybe "no-theme" $ customThemeDark <|> Map.lookup "theme-dark.css" staticFiles
      Light -> fromMaybe "no-theme" $ customThemeLight <|> Map.lookup "theme-light.css" staticFiles
#endif


-- | Toggle the frontend theme by triggering the event handler of `ThemeChanged`
-- in the frontend. A fade-in animation is played when the theme toggled, and it needs
-- to be removed by the frontend.
toggleTheme :: SessionId -> ConfirmLogin -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
toggleTheme sessionId _ = do
  theme <- Session.getSessionTheme sessionId
  case theme of
    Theme.Light -> Session.setSessionTheme sessionId Theme.Dark
    Theme.Dark  -> Session.setSessionTheme sessionId Theme.Light
  html <- index sessionId
  pure $ addHeader ThemeChanged (html `with` [ class_ "fade-in " ])


changeLocale :: SessionId -> Maybe Locale -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
changeLocale _ Nothing = throwError (FilehubError LocaleError "Invalid locale")
changeLocale sessionId (Just locale) = do
  Session.setSessionLocale sessionId locale
  addHeader LocaleChanged <$> index sessionId


serve :: SessionId -> ConfirmLogin -> Maybe ClientPath
      -> Filehub (Headers '[ Header "Content-Type" String
                           , Header "Content-Disposition" String
                           , Header "Cache-Control" String
                           ]
                           (ConduitT () ByteString (ResourceT IO) ()))
serve sessionId _ mFile = do
  storage    <- Session.getStorage sessionId
  root       <- Session.getRoot sessionId
  clientPath <- withQueryParam mFile
  let path   = ClientPath.fromClientPath root clientPath
  storage.get path >>= \case
    Just file -> do
      conduit <- storage.readStream file
      pure
        . addHeader (ByteString.unpack file.mimetype)
        . addHeader (printf "inline; filename=%s" (takeFileName path))
        . addHeader "public, max-age=31536000, immutable"
        $ conduit
    Nothing -> do
      throwError (FilehubError InvalidPath "file path is invalid")


thumbnail :: SessionId -> ConfirmLogin -> Maybe ClientPath
          -> Filehub (Headers '[ Header "Content-Type" String
                               , Header "Content-Disposition" String
                               , Header "Cache-Control" String
                               ]
                               (ConduitT () ByteString (ResourceT IO) ()))
thumbnail sessionId _ mFile = do
  storage    <- Session.getStorage sessionId
  root       <- Session.getRoot sessionId
  clientPath <- withQueryParam mFile
  let path   = ClientPath.fromClientPath root clientPath

  storage.get path >>= \case
    Just file -> do
      conduit <- serveOriginal storage file
      pure
        . addHeader (ByteString.unpack file.mimetype)
        . addHeader (printf "inline; filename=%s" (takeFileName path))
        . addHeader "public, max-age=31536000, immutable"
        $ conduit
    Nothing -> do
      throwError (FilehubError InvalidPath "thumbnail file path is invalid")

  where
    serveOriginal storage file =
      if
        | file.mimetype `isMime` "image" -> storage.readStream file
        | otherwise                      -> throwError (FilehubError FormatError "Invalid mime type for thumbnail")



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
static :: [FilePath] -> Filehub (Headers '[ Header "Content-Type" String, Header "Cache-Control" String ] ByteString)
static paths = do
#ifdef DEBUG
  dir          <- liftIO $ Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data/filehub")
  let path     =  dir </> List.intercalate "/" paths
  let mimetype =  Mime.defaultMimeLookup (Text.pack path)
  content      <- liftIO . readFile $ path
  pure
    . addHeader (ByteString.unpack mimetype)
    . addHeader "public, max-age=31536000, immutable"
    $ content
#else
  let path = List.intercalate "/" paths
  case Map.lookup path staticFiles of
    Just content -> do
      let mimetype = Mime.defaultMimeLookup (Text.pack path)
      pure
        . addHeader (ByteString.unpack mimetype)
        . addHeader "public, max-age=31536000, immutable"
        $ content
    Nothing -> throwError do HTTPError (err404 { errBody = [i|File doesn't exist|]})
#endif



#ifdef DEBUG
-- | Storybook for debug purposes.
preview :: Maybe Text -> Maybe Text -> Filehub (Html ())
preview mStory mDisplay = do
  let ctx = TemplateContext
        { readOnly           = False
        , noLogin            = False
        , display            = fromMaybe Desktop $ fmap (\case "mobile" -> Mobile; _ -> Desktop) mDisplay
        , layout             = ThumbnailLayout
        , theme              = Dark
        , sortedBy           = ByNameDown
        , selected           = NoSelection
        , state              = ControlPanelDefault
        , root               = ""
        , locale             = EN
        , currentDir         = ""
        , currentTarget      = undefined
        , simpleAuthUserDB   = undefined
        , oidcAuthProviders  = undefined
        }

  let content = do
        doctypehtml_ $ do
          Template.withDefault ctx.display "#000000" do
            style_ (Text.decodeUtf8 $ fromMaybe "no-theme" $ Map.lookup "theme-light.css" staticFiles)
            case mStory of
              Nothing -> mempty
              Just "editor" -> do
                case ctx.display of
                  Mobile    -> runTemplate ctx $ Template.Mobile.editorModal "filename" "File content"
                  Desktop   -> runTemplate ctx $ Template.Desktop.editorModal "filename" "File content"
                  NoDisplay -> mempty
              Just "new-folder" -> runTemplate ctx Template.Desktop.newFolderModal
              Just "new-file" -> runTemplate ctx Template.Desktop.newFileModal
              Just "locale-button" -> Template.Desktop.localeBtn
              _ -> "unknown story"

  pure do
    html_ do
      head_ do
        style_ previewCSS

      body_ do
        div_ [ id_ "preview-side-bar" ] do
          ul_ do
            li_ do a_ [ href_ "/preview?story=editor&display=desktop" ] "editor"
            li_ do a_ [ href_ "/preview?story=new-folder&display=desktop" ] "new-folder"
            li_ do a_ [ href_ "/preview?story=new-file&display=desktop" ] "new-file"
            li_ do a_ [ href_ "/preview?story=locale-button&display=desktop" ] "locale-button"
        div_ [ id_ "preview-container"] do
          iframe_ [ id_ "preview-frame"
                  , srcdoc_ (LText.toStrict (renderText content))
                  , sandbox_ "allow-same-origin allow-scripts" ]
                  mempty

  where
    previewCSS =
      [iii|
        body { margin: 0; height: 100vh; display: flex; flex-direction: row; font-family: sans-serif; }
        a { text-decoration: none; color: white; }
        \#preview-side-bar { width: 200px; background-color: \#222;
          color: white; display: flex; flex-direction: column; padding: 1rem; box-sizing: border-box; }
        \#preview-side-bar ul { list-style: none; padding: 0; margin: 0; }
        \#preview-side-bar li { padding: 0.5rem 0; cursor: pointer; }
        \#preview-side-bar li:hover { background-color: \#444; }
        \#preview-container { flex: 1; display: flex; justify-content: center;
          align-items: center; background-color: \#f0f0f0; padding: 1rem; box-sizing: border-box; }
        \#preview-frame { width: 80%; height: 80%; border: 1px solid \#ccc;
          border-radius: 8px; box-shadow: 0 0 10px rgba(0,0,0,0.1); background: white; }
      |]

#endif


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
  $ server
  where
    ctx = Filehub.Handler.sessionHandler env
        :. Filehub.Handler.readOnlyHandler env
        :. Filehub.Handler.desktopOnlyHandler env
        :. Filehub.Handler.mobileOnlyHandler env
        :. Filehub.Handler.loginHandler env
        :. Filehub.Handler.sharedLinkPermitHandler env
        :. EmptyContext
