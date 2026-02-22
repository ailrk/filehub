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
import Conduit (ConduitT, ResourceT, yield)
import Conduit qualified
import Control.Applicative (Alternative((<|>)))
import Control.Monad (forM, void, when, join, replicateM)
import Control.Monad.Fix (fix)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (object, KeyValue (..), (.:), withObject, Value)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as Char
import Data.ClientPath (ClientPath (..), AbsPath (..), (<./>), Root)
import Data.ClientPath qualified as ClientPath
import Data.ClientPath.Effectful (validateAbsPath)
import Data.Coerce (coerce)
import Data.File (FileType(..), File(..), FileContent (..), withContent, defaultFileWithContent, FileInfo)
import Data.FileEmbed qualified as FileEmbed
import Data.Foldable (forM_)
import Data.Function (fix, (&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i, iii)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID qualified as UUID
import Effectful (withRunInIO, runEff, MonadIO (..), raise, Eff )
import Effectful.Concurrent.Async (async, forConcurrently_)
import Effectful.Concurrent.STM (newTVarIO, readTBQueue, writeTBQueue, atomically, modifyTVar', readTVar, isEmptyTBQueue, TBQueue, TVar)
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logInfo_, logAttention_)
import Effectful.Reader.Dynamic (asks)
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
import Filehub.Server.Desktop qualified as Server.Desktop
import Filehub.Server.Mobile qualified as Server.Mobile
import Filehub.Server.Internal (withQueryParam, parseHeader')
import Filehub.Server.Internal qualified as Server.Internal
import Filehub.Session (SessionId(..), TargetView (..))
import Filehub.Session qualified as Session
import Filehub.Session.Effectful (runSessionEff, SessionGet(..))
import Filehub.Session.Effectful qualified as Session
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
import Filehub.Types ( ControlPanelState (..) , Display (..) , Layout (..) , LoginForm(..) , NewFile(..) , NewFolder(..) , OpenTarget , Resolution , SearchWord , Selected (..) , SortFileBy(..) , Theme(..) , UIComponent (..) , UpdatedFile(..) , UpdatedFile(..) , FilehubEvent (..), RenameFile (..), CopyState (..), TargetSessionData (..), Selected(..), MoveFile (..), Resource (..))
import Lens.Micro ((&), (.~), (?~), (<&>))
import Lucid
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
import Servant (Application , Context (..) , Header , Headers , NoContent (..) , addHeader , err301 , err303 , err400 , err404 , err500 , errHeaders , noHeader , serveWithContextT , errBody)
import Servant (Headers, Header, addHeader)
import Servant.API.EventStream (RecommendedEventSourceHeaders, recommendedEventSourceHeaders)
import Servant.Multipart (MultipartData(..), Mem)
import Servant.Server.Generic (AsServerT)
import System.Directory (removeFile)
import System.FilePath (takeFileName, (</>), makeRelative, takeDirectory)
import System.IO.Temp qualified as Temp
import System.Random (randomRIO)
import Target.Types (TargetId, AnyTarget)
import Target.Types qualified as Target
import Text.Printf (printf)
import UnliftIO.Exception (SomeException, catch)
import Web.Cookie (SetCookie (..), defaultSetCookie)
import Worker.Task (TaskId, newTaskId)
import Debug.Trace (traceShowM)

#ifdef DEBUG
import Effectful (MonadIO (liftIO))
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


server :: IsFilehub es => Api (AsServerT (Eff es))
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
  , rename                = rename
  , delete                = delete
  , newFolder             = newFolder
  , renameModal           = renameModal
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
  , toggleSidebar         = toggleSidebar
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


healthz :: SessionId -> Eff es Text
healthz _ = do
  let gitRev = Text.dropAround Char.isSpace
             $ Text.decodeUtf8 $(Filehub.QQ.getGitRev)
  pure $ Text.concat [ gitRev, ", ok." ]


initialize :: IsFilehub es => SessionId -> Resolution -> Eff es (Html ())
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
home :: IsFilehub es => SessionId -> ConfirmLogin -> Eff es (Html ())
home sessionId _  = runSessionEff sessionId do
  display <- Session.get (.display)
  m <- raise manifest
  let background
        = fromMaybe "#000000"
        . flip parseMaybe m
        . withObject "manifest"
        $ (.: "theme_color")
  raise $ Server.Internal.clear sessionId
  case display of
    NoDisplay -> pure Template.bootstrap
    -- Index is initially hidden, the frontend will play an intro animation, set
    -- the opacity to 1.
    -- `home` is the only endpoint that needs to play the animation, so it's important
    -- that the animation classes are removed from the js.
    Desktop -> do
      html <- raise $ Server.Desktop.index sessionId
      pure $ Template.withDefault display background do
        html `with` [ class_ "hidden fade-in " ]
    Mobile -> do
      html <- raise $ Server.Mobile.index sessionId
      pure $ Template.withDefault display background do
        html `with` [ class_ "hidden fade-in" ]


-- | Force to refresh a component. It's useful for the client to selectively update ui.
refresh :: IsFilehub es => SessionId -> ConfirmLogin -> Maybe UIComponent -> Eff es (Html ())
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
-- same conduit; when there are no pending task, the conduit finshes; when there is not task,
-- no conduit.
listen :: IsFilehub es => SessionId -> ConfirmLogin -> Eff es (RecommendedEventSourceHeaders (ConduitT () Notification IO ()))
listen sessionId _ = recommendedEventSourceHeaders <$> runSessionEff sessionId do
  notifications <- Session.get (.notifications)
  pendingTasks <- Session.get (.pendingTasks)
  streamAtomically \loop -> do
    n <- readTBQueue notifications
    case n of
      TaskCompleted taskId -> do
        modifyTVar' pendingTasks (Set.delete taskId)
        tasksRemaining <- readTVar pendingTasks
        if Set.null tasksRemaining
           then do
             clearQueue notifications
             pure (yield n)
           else pure do yield n; loop
      DeleteProgressed _ _ -> pure do yield n; loop
      PasteProgressed _ _  -> pure do yield n; loop
      MoveProgressed _ _   -> pure do yield n; loop
      UploadProgressed _ _ -> pure do yield n; loop
      Pong                 -> pure do yield n; loop
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


-- | Return the login page
loginPage :: IsFilehub es => SessionId -> Maybe Text -> Maybe Text -> Eff es (Html ())
loginPage sessionId cookie Nothing = runSessionEff sessionId do
  ctx@TemplateContext { noLogin } <- raise $ makeTemplateContext sessionId
  if noLogin
     then go
     else do
       case fmap Text.encodeUtf8 cookie >>= parseHeader' >>= Cookies.fromCookies of
         Just authId' -> do
           authId <- Session.get (.authId)
           if authId == Just authId'
              then go
              else pure $ runTemplate ctx Template.Login.login
         Nothing -> pure $ runTemplate ctx Template.Login.login
  where
    go = throwError do HTTPError (err301 { errHeaders = [(hLocation, "/")] })
loginPage sessionId _ (Just _) = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Login.login


loginToggleTheme :: IsFilehub es => SessionId -> Eff es (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
loginToggleTheme sessionId = runSessionEff sessionId do
  theme <- Session.get (.theme)
  case theme of
    Theme.Light -> Session.set (.theme) Theme.Dark
    Theme.Dark  -> Session.set (.theme) Theme.Light
  ctx <- raise $ makeTemplateContext sessionId
  let html = runTemplate ctx Template.Login.login'
  pure $ addHeader ThemeChanged html


loginChangeLocale :: IsFilehub es => SessionId -> Maybe Locale -> Eff es (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
loginChangeLocale _ Nothing = throwError (FilehubError LocaleError "Invalid locale")
loginChangeLocale sessionId (Just locale) = runSessionEff sessionId do
  Session.set (.locale) locale
  ctx <- raise $ makeTemplateContext sessionId
  let html = runTemplate ctx Template.Login.login'
  pure $ addHeader LocaleChanged html


-- | Handle the simple authetication login.
loginAuthSimple :: IsFilehub es => SessionId -> LoginForm
                -> Eff es (Headers '[ Header "Set-Cookie" SetCookie
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


loginAuthOIDCRedirect :: IsFilehub es => SessionId -> Text -> Eff es NoContent
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


loginAuthOIDCCallback :: IsFilehub es
                      => SessionId
                      -> Maybe Text
                      -> Maybe Text
                      -> Maybe Text
                      -> Maybe Text
                      -> Maybe Text
                      -> Maybe Text
                      -> Eff es NoContent
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


logout :: IsFilehub es
       => SessionId -> ConfirmLogin -> Eff es (Headers '[ Header "Set-Cookie" SetCookie
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
    Just (setCookie, authId) -> runSessionEff sessionId do
      Session.set (.authId) Nothing
      Session.set (.oidcFlow) Nothing
      ActiveUser.Pool.delete authId
      addHeader
        (setCookie
          { setCookieExpires = Just (UTCTime (fromGregorian 1970 1 1) 0) })
        . addHeader "/login"
        <$> pure NoContent
    Nothing -> noHeader . noHeader <$> pure NoContent


cd :: IsFilehub es
   => SessionId -> ConfirmLogin -> Maybe ClientPath
   -> Eff es (Headers '[ Header "HX-Trigger-After-Swap" FilehubEvent ] (Html ()))
cd sessionId _ mClientPath = runSessionEff sessionId do
  root       <- Session.get (.root)
  storage    <- Session.get (.storage)
  clientPath <- raise $ withQueryParam mClientPath
  storage.cd (ClientPath.fromClientPath root clientPath)
  html <- do
    toolBar' <- toolBar sessionId
    view'    <- view sessionId
    pure do
      toolBar' `with` [ term "hx-swap-oob" "true" ]
      view'
  pure $ addHeader DirChanged html


delete :: IsFilehub es
       => SessionId -> ConfirmLogin -> ConfirmReadOnly -> [ClientPath] -> Bool
       -> Eff es (Headers '[ Header "X-Filehub-Selected-Count" Int
                           , Header "HX-Trigger" FilehubEvent
                           ] (Html ()))
delete sessionId _ _ clientPaths deleteSelected = runSessionEff sessionId do
  root          <- Session.get (.root)
  storage       <- Session.get (.storage)
  notifications <- Session.get (.notifications)
  count         <- length <$> Selected.allSelecteds sessionId
  taskId        <- newTaskId
  deleteCounter <- newTVarIO @_ @Integer 0

  void $ async do
    atomically do writeTBQueue notifications (DeleteProgressed taskId 0)

    do
      forConcurrently_ clientPaths \clientPath -> do
        let path = ClientPath.fromClientPath root clientPath
        storage.delete path
        atomically do
          modifyTVar' deleteCounter (+ 1)
          n <- readTVar deleteCounter
          writeTBQueue notifications (DeleteProgressed taskId (n % max 1 (fromIntegral count)))

    when deleteSelected do
      allSelecteds <- Selected.allSelecteds sessionId
      forM_ allSelecteds \(target, selected) -> do
        Session.withTarget (Target.getTargetId target) do
          case selected of
            NoSelection -> pure ()
            Selected x xs -> do
              forConcurrently_ (fmap (ClientPath.fromClientPath root) (x:xs)) \path -> do
                storage.delete path
                atomically do
                  modifyTVar' deleteCounter (+ 1)
                  n <- readTVar deleteCounter
                  writeTBQueue notifications (DeleteProgressed taskId (n % max 1 (fromIntegral count)))

    atomically do writeTBQueue notifications (TaskCompleted taskId)
  raise $ Server.Internal.clear sessionId
  newCount <- length <$> Selected.allSelecteds sessionId
  addHeader newCount . addHeader SSEStarted <$> raise (index sessionId)


rename
  :: IsFilehub es
  => SessionId -> ConfirmLogin -> ConfirmReadOnly
  -> RenameFile
  -> Eff es (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
rename sessionId _ _ (RenameFile old new) = runSessionEff sessionId do
  storage <- Session.get (.storage)
  root    <- Session.get (.root)
  traceShowM "--rename"
  traceShowM root
  traceShowM old
  traceShowM new
  traceShowM (ClientPath.fromClientPath root old)
  storage.rename
    (ClientPath.fromClientPath root old)
    new
  html <- raise $ view sessionId
  pure $ addHeader FileRenamed html


newFile :: IsFilehub es => SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFile -> Eff es (Html ())
newFile sessionId _ _ (NewFile name) = runSessionEff sessionId do
  storage     <- Session.get (.storage)
  AbsPath dir <- Session.get (.currentDir)
  path        <- validateAbsPath (dir </> Text.unpack name) (FilehubError InvalidPath ("<redacted>/" <> show name))
  storage.new path
  view sessionId


updateFile :: IsFilehub es => SessionId -> ConfirmLogin -> ConfirmReadOnly -> UpdatedFile -> Eff es (Html ())
updateFile sessionId _ _ (UpdatedFile clientPath content) = runSessionEff sessionId do
  storage <- Session.get (.storage)
  root    <- Session.get (.root)
  let path  = ClientPath.fromClientPath root clientPath
  storage.write $ defaultFileWithContent
    { path     = path
    , content  = FileContentRaw (Text.encodeUtf8 content)
    }
  raise $ view sessionId


newFolder :: IsFilehub es => SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFolder -> Eff es (Html ())
newFolder sessionId _ _ (NewFolder name) = runSessionEff sessionId do
  AbsPath dir <- Session.get (.currentDir)
  storage     <- Session.get (.storage)
  path <- validateAbsPath
            (dir </> Text.unpack name)
            (FilehubError InvalidPath ("<redacted>/" <> show name))
  storage.newFolder path
  raise $ view sessionId


copy :: IsFilehub es => SessionId -> ConfirmLogin -> ConfirmReadOnly -> Eff es (Html ())
copy sessionId _ _ = do
  Copy.select sessionId
  Copy.copy sessionId
  controlPanel sessionId


copy1 :: IsFilehub es => SessionId -> ConfirmLogin -> ConfirmReadOnly -> Maybe ClientPath -> Eff es (Html ())
copy1 sessionId _ _ mClientPath = do
  clientPath <- withQueryParam mClientPath
  Server.Internal.clear sessionId
  Selected.setSelected sessionId (Selected clientPath [])
  Copy.select sessionId
  Copy.copy sessionId
  index sessionId


type TargetFrom  = AnyTarget
type TargetTo    = AnyTarget
type Destination = AbsPath


data PasteTask
  = PasteFile TargetFrom TargetTo FileInfo Destination
  | PasteDir TargetTo Destination [PasteTask]


paste :: IsFilehub es
      => SessionId -> ConfirmLogin -> ConfirmReadOnly
      -> Eff es (Headers '[ Header "X-Filehub-Selected-Count" Int
                          , Header "HX-Trigger" FilehubEvent
                          ] (Html ()))
paste sessionId _ _ = runSessionEff sessionId do
  notifications <- Session.get (.notifications)
  pasteCounter  <- newTVarIO @_ @Integer 0
  taskId        <- newTaskId
  state         <- Copy.getCopyState sessionId
  case state of
    Paste selections -> do
      tasks <- do
        TargetView to sessionData <- Session.get (.currentTarget)
        createPasteTasks sessionData.currentDir to selections
      let taskCount = fromIntegral (length tasks)

      (void . async) do
        forConcurrently_ tasks $ fix \rec task -> do
          case task of
            PasteFile from to file dst -> do
              let fromId = Target.getTargetId from
              let toId   = Target.getTargetId to
              conduit <- Session.withTarget fromId do
                storage <- Session.get (.storage)
                storage.readStream file
              Session.withTarget toId do
                storage <- Session.get (.storage)
                storage.write $ file
                  & flip withContent (FileContentConduit conduit)
                  & #path .~ dst
              atomically do
                modifyTVar' pasteCounter (+ 1)
                n <- readTVar pasteCounter
                writeTBQueue notifications (PasteProgressed taskId (n % max 1 taskCount) )

            PasteDir to dst subTasks -> do
              Session.withTarget (Target.getTargetId to) do
                storage <- Session.get (.storage)
                storage.newFolder dst
              forConcurrently_ subTasks rec

        Copy.setCopyState sessionId NoCopyPaste
        Selected.clearSelectedAllTargets sessionId
        atomically $ writeTBQueue notifications (TaskCompleted taskId)
    _ -> do
      logAttention_ [i|[v8dsaz] #{sessionId}, not in pastable state.|]
      throwError (FilehubError SelectError "Not in a pastable state")

  raise $ Server.Internal.clear sessionId
  selectedCount <- length <$> Selected.allSelecteds sessionId
  addHeader selectedCount . addHeader SSEStarted <$> raise (index sessionId)

  where
    createPasteTasks fromDir to selections = fmap (mconcat . mconcat) do
      forM selections \(from, files) -> do
        forM files $ flip fix fromDir \rec (AbsPath currentDir) file -> do
          let name  =  coerce takeFileName file.path
          let fromId = Target.getTargetId from
          dst <- validateAbsPath (currentDir </> takeFileName name) (FilehubError InvalidPath "")
          case file.content of
            Regular -> pure [ PasteFile from to file dst ]
            Dir -> runSessionEff sessionId do
              Session.withTarget fromId do
                storage <- Session.get (.storage)
                (TargetView _ (TargetSessionData { currentDir = savedDir })) <- Session.get (.currentTarget)
                storage.cd file.path
                result <- do
                  dirFiles <- storage.lsCwd
                  forM dirFiles \dfile -> raise $ rec dst dfile
                storage.cd savedDir -- go back
                pure [ PasteDir to dst (mconcat result) ]


move :: IsFilehub es
     => SessionId -> ConfirmLogin -> ConfirmReadOnly -> MoveFile
     -> Eff es (Headers '[ Header "HX-Trigger" FilehubEvent
                         , Header "HX-Trigger" FilehubEvent
                         ] (Html ()))
move sessionId _ _ (MoveFile src tgt) = runSessionEff sessionId do
  storage       <- Session.get (.storage)
  root          <- Session.get (.root)
  notifications <- Session.get (.notifications)
  taskId        <- newTaskId
  let srcPaths  =  fmap (ClientPath.fromClientPath root) src
  let tgtPath   =  ClientPath.fromClientPath root tgt

  -- check before take action
  forM_ srcPaths \srcPath -> do
    isTgtDir <- storage.isDirectory tgtPath
    when (not isTgtDir) do
      throwError (FilehubError InvalidDir "Target is not a directory")

    when (srcPath == tgtPath)  do
      throwError (FilehubError InvalidDir "Can't move to the same directory")

    when (coerce takeDirectory srcPath == tgtPath)  do
      throwError (FilehubError InvalidDir "Already in the current directory")

    let dstPath = tgtPath <./> coerce takeFileName srcPath
    mFile <- storage.get dstPath
    when (isJust mFile) do
      throwError (FilehubError InvalidPath "The destination already exists")

  void $ async do
    atomically do
      writeTBQueue notifications (MoveProgressed taskId 0)

    storage.mv do
      fmap (\srcPath -> (srcPath, tgtPath <./> coerce takeFileName srcPath)) srcPaths

    atomically do
      writeTBQueue notifications  (TaskCompleted taskId)

  raise $ Server.Internal.clear sessionId
  addHeader FileMoved . addHeader SSEStarted <$> (raise $ index sessionId)


renameModal :: IsFilehub es
            => SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly
            -> Maybe ClientPath -> Eff es (Html ())
renameModal sessionId _ _ _ mClientPath = runSessionEff sessionId do
  root       <- Session.get (.root)
  clientPath <- raise $ withQueryParam mClientPath
  ctx        <- raise $ makeTemplateContext sessionId
  pure $ runTemplate ctx (Template.Desktop.renameModal (ClientPath.fromClientPath root clientPath))


newFileModal :: IsFilehub es => SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Eff es (Html ())
newFileModal sessionId _ _ _ = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.newFileModal


newFolderModal :: IsFilehub es => SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Eff es (Html ())
newFolderModal sessionId  _ _ _ = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.newFolderModal


fileDetailModal :: IsFilehub es => SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> Maybe ClientPath -> Eff es (Html ())
fileDetailModal sessionId _ _ mPath = do
  Server.Desktop.fileDetailModal sessionId mPath


editorModal :: IsFilehub es => SessionId -> ConfirmLogin -> Maybe ClientPath -> Eff es (Html ())
editorModal sessionId _ mClientPath = runSessionEff sessionId do
  display <- Session.get (.display)
  case display of
    Mobile    -> Server.Mobile.editorModal sessionId mClientPath
    Desktop   -> Server.Desktop.editorModal sessionId mClientPath
    NoDisplay -> error "impossible"


selectLayout :: IsFilehub es => SessionId -> ConfirmLogin -> Maybe Layout -> Eff es (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
selectLayout sessionId _ layout = runSessionEff sessionId do
  Session.set (.layout) (fromMaybe ThumbnailLayout layout)
  addHeader LayoutChanged <$> (raise $ index sessionId)


sortTable :: IsFilehub es => SessionId -> ConfirmLogin -> Maybe SortFileBy -> Eff es (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
sortTable sessionId _ order = runSessionEff sessionId do
  display <- Session.get (.display)
  Session.set (.sortedFileBy) (fromMaybe ByNameUp order)
  html <- do
    view' <- raise $ view sessionId
    case display of
      Mobile -> do
        toolBar' <- raise $ Server.Mobile.toolBar sessionId
        pure do
          toolBar' `with` [ term "hx-swap-oob" "true" ]
          view'
      Desktop   -> pure view'
      NoDisplay -> pure view'
  pure $ addHeader TableSorted html


search :: IsFilehub es => SessionId -> ConfirmLogin -> SearchWord -> Eff es (Html ())
search sessionId _ searchWord = runSessionEff sessionId do
  storage <- Session.get (.storage)
  display <- Session.get (.display)
  ctx <- raise $ makeTemplateContext sessionId
  files   <- storage.lsCwd
  case display of
    Mobile    -> pure $ runTemplate ctx (Template.search searchWord files Template.Mobile.table)
    Desktop   -> pure $ runTemplate ctx (Template.search searchWord files Template.Desktop.table)
    NoDisplay -> error "impossible"


download :: IsFilehub es
         => SessionId -> ConfirmLogin -> [ClientPath]
         -> Eff es (Headers '[ Header "Content-Disposition" String ] (ConduitT () ByteString (ResourceT IO) ()))
download sessionId _ clientPaths = runSessionEff sessionId do
  root    <- Session.get (.root)
  storage <- Session.get (.storage)
  case clientPaths of
    [clientPath@(ClientPath path)] -> do
      mFile   <- storage.get (ClientPath.fromClientPath root clientPath)
      conduit <- storage.download clientPath
      case mFile of
        Just file -> do
          let filename =
                case file.content of
                  Regular -> printf "attachement; filename=%s" (takeFileName path)
                  Dir     -> printf "attachement; filename=%s.zip" (takeFileName path)
          pure $ addHeader filename conduit
        Nothing -> do
          throwError (FilehubError InvalidPath "can't download, invalid file path")
    _ -> do
      (zipPath, _) <- liftIO do
        tempDir <- Temp.getCanonicalTemporaryDirectory
        Temp.openTempFile tempDir "DXXXXXX.zip"

      files <- traverse (storage.get . ClientPath.fromClientPath root) clientPaths <&> catMaybes

      tasks <- forM files \file -> do
        conduit <- storage.readStream file
        pure (file.path, conduit)

      Zip.createArchive zipPath do
        forM_ tasks \(path, conduit) -> do
          m <- Zip.mkEntrySelector  (coerce makeRelative root path)
          Zip.sinkEntry Zip.Zstd conduit m
      tag <- Text.pack <$> replicateM 8 (randomRIO ('a', 'z'))
      let conduit =
            Conduit.bracketP
              (pure ())
              (\_ -> removeFile zipPath)
              (\_ -> Conduit.sourceFile zipPath)
      pure $ addHeader (printf "attachement; filename=%s.zip" tag) conduit


upload :: IsFilehub es
       => SessionId -> ConfirmLogin -> ConfirmReadOnly -> MultipartData Mem
       -> Eff es (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
upload sessionId _ _ multipart = runSessionEff sessionId do
  notifications <- Session.get (.notifications)
  taskId        <- newTaskId
  uploadCounter <- newTVarIO @_ @Integer 0
  let taskCount =  fromIntegral $ length multipart.files

  void $ async do
    atomically do
      writeTBQueue notifications (UploadProgressed taskId 0)


    runSessionEff sessionId do
      storage <- Session.get (.storage)
      forConcurrently_ multipart.files \filedata -> do
        storage.upload filedata
        atomically do
          modifyTVar' uploadCounter (+ 1)
          n <- readTVar uploadCounter
          writeTBQueue notifications (UploadProgressed taskId (n % max 1 taskCount) )

      atomically do
        writeTBQueue notifications (UploadProgressed taskId 1)
        writeTBQueue notifications (TaskCompleted taskId)
  addHeader SSEStarted <$> (raise $ index sessionId)


selectRows :: IsFilehub es => SessionId -> ConfirmLogin -> Selected -> Eff es (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
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


contextMenu :: IsFilehub es => SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> [ClientPath] -> Eff es (Html ())
contextMenu sessionId _ _ paths = Server.Desktop.contextMenu sessionId paths


cancel :: IsFilehub es => SessionId -> ConfirmLogin -> Eff es (Headers '[Header "X-Filehub-Selected-Count" Int] (Html ()))
cancel sessionId _ = do
  Server.Internal.clear sessionId
  count <- length <$> Selected.allSelecteds sessionId
  addHeader count <$> index sessionId


initViewer :: IsFilehub es
           => SessionId -> ConfirmLogin -> Maybe ClientPath
           -> Eff es (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
initViewer sessionId _ mClientPath = runSessionEff sessionId do
  root       <- Session.get (.root)
  order      <- Session.get (.sortedFileBy)
  storage    <- Session.get (.storage)
  clientPath <- raise  $ withQueryParam mClientPath
  payload <- do
    let filePath  =  ClientPath.fromClientPath root clientPath
    let dir       =  coerce takeDirectory filePath
    files         <- takeResourceFiles . Sort.sortFiles order <$> (storage.ls dir)
    let idx       =  fromMaybe 0 $ List.elemIndex filePath (fmap (.path) files)
    let resources =  fmap (toResource root) files
    pure $ ViewerInited resources idx
  pure $ addHeader payload NoContent
  where
    isResource :: MimeType -> Bool
    isResource s = any (s `isMime`)  ["image", "video", "audio"]

    takeResourceFiles :: [FileInfo] -> [FileInfo]
    takeResourceFiles = filter (isResource . (.mimetype))

    toResource :: Root -> FileInfo -> Resource
    toResource root f =
      Resource
        { url = let ClientPath path = ClientPath.toClientPath root f.path -- encode path url
                 in ClientPath.RawClientPath [i|/serve?file=#{path}|]
                                                  , mimetype = Text.decodeUtf8 f.mimetype
        }


open :: IsFilehub es
     => SessionId -> ConfirmLogin -> Maybe OpenTarget -> Maybe ClientPath
     -> Eff es (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
open _ _ mTarget mClientPath = do
  clientPath <- withQueryParam mClientPath
  target     <- withQueryParam mTarget
  pure $ addHeader (Opened target clientPath) NoContent


changeTarget :: IsFilehub es
             => SessionId -> ConfirmLogin -> Maybe TargetId
             -> Eff es (Headers '[Header "HX-Trigger-After-Swap" FilehubEvent] (Html ()))
changeTarget sessionId _ mTargetId = runSessionEff sessionId do
  savedTargetId <- do
    TargetView saved _ <- Session.get (.currentTarget)
    pure $ Target.getTargetId saved

  let restore = Session.set (.currentTarget) savedTargetId
  targetId <- raise $ withQueryParam mTargetId
  Session.set (.currentTarget) targetId

  html <- withRunInIO \unlift -> do
    unlift (raise $ index sessionId)
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
shared :: IsFilehub es => SessionId -> Maybe SharedLinkPermit -> SharedLinkHash -> Maybe ClientPath -> Eff es (Html ())
shared sessionId mClientPermit hash mClientPath = runSessionEff sessionId do
  case mClientPermit of
    Just clientPermit -> do
      sharedLinkPermit <- Session.get (.sharedLinkPermit)
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
    goAuth = throwError do
      HTTPError err303 { errHeaders = [( "Location" , "/s/auth")] }

    sharedIndex = do
      pure mempty


sharedAuth :: SessionId -> Eff es (Html ())
sharedAuth sessionId = undefined


themeCss :: IsFilehub es => SessionId -> Eff es ByteString
themeCss sessionId = runSessionEff sessionId do
  theme <- Session.get (.theme)
  customThemeDark  <- (fmap . fmap) Theme.customTheme2Css (asks @Env (.customThemeDark))
  customThemeLight <- (fmap . fmap) Theme.customTheme2Css (asks @Env (.customThemeLight))
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


-- | Toggle the frontend theme by triggering the event handler of `ThemeChanged`
-- in the frontend. A fade-in animation is played when the theme toggled, and it needs
-- to be removed by the frontend.
toggleTheme :: IsFilehub es => SessionId -> ConfirmLogin -> Eff es (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
toggleTheme sessionId _ = runSessionEff sessionId do
  theme <- Session.get (.theme)
  case theme of
    Theme.Light -> Session.set (.theme) Dark
    Theme.Dark  -> Session.set (.theme) Light
  html <- index sessionId
  pure $ addHeader ThemeChanged (html `with` [ class_ "fade-in " ])


changeLocale :: IsFilehub es => SessionId -> Maybe Locale -> Eff es (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
changeLocale _ Nothing = throwError (FilehubError LocaleError "Invalid locale")
changeLocale sessionId (Just locale) = runSessionEff sessionId do
  Session.set(.locale) locale
  addHeader LocaleChanged <$> (index sessionId)


toggleSidebar :: IsFilehub es => SessionId -> ConfirmLogin -> Eff es (Html ())
toggleSidebar sessionId _ = runSessionEff sessionId do
  b <- Session.get (.sidebarCollapsed)
  Session.set (.sidebarCollapsed) (not b)
  index sessionId


serve :: IsFilehub es
      => SessionId -> ConfirmLogin -> Maybe ClientPath
      -> Eff es (Headers '[ Header "Content-Type" String
                          , Header "Content-Disposition" String
                          , Header "Cache-Control" String
                          ]
                          (ConduitT () ByteString (ResourceT IO) ()))
serve sessionId _ mFile = runSessionEff sessionId do
  root       <- Session.get (.root)
  storage    <- Session.get (.storage)
  clientPath <- withQueryParam mFile
  let path   = ClientPath.fromClientPath root clientPath
  storage.get path >>= \case
    Just file -> do
      conduit <- storage.readStream file
      pure
        . addHeader (ByteString.unpack file.mimetype)
        . addHeader (printf "inline; filename=%s" (coerce takeFileName path :: String))
        . addHeader "public, max-age=31536000, immutable"
        $ conduit
    Nothing -> do
      throwError (FilehubError InvalidPath "file path is invalid")


thumbnail :: IsFilehub es
          => SessionId -> ConfirmLogin -> Maybe ClientPath
          -> Eff es (Headers '[ Header "Content-Type" String
                              , Header "Content-Disposition" String
                              , Header "Cache-Control" String
                              ]
                              (ConduitT () ByteString (ResourceT IO) ()))
thumbnail sessionId _ mFile = runSessionEff sessionId do
  root       <- Session.get (.root)
  storage    <- Session.get (.storage)
  clientPath <- withQueryParam mFile
  let path   = ClientPath.fromClientPath root clientPath

  storage.get path >>= \case
    Just file -> do
      conduit <- serveOriginal storage file
      pure
        . addHeader (ByteString.unpack file.mimetype)
        . addHeader (printf "inline; filename=%s" (coerce takeFileName path :: String))
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
manifest :: Eff es Value
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
static :: IsFilehub es
       => [FilePath] -> Eff es (Headers '[ Header "Content-Type" String
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
    Nothing -> throwError do HTTPError (err404 { errBody = [i|File doesn't exist|]})
#endif

  let mimetype = Mime.defaultMimeLookup (Text.pack path)
  let etag    = "\"" <> ByteString.unpack (Base64.encode (SHA256.hash content)) <> "\""
  pure
    . addHeader (ByteString.unpack mimetype)
    . addHeader "public, no-cache"
    . addHeader etag
    $ content


------------------------------------
-- Components
------------------------------------


index :: IsFilehub es => SessionId -> Eff es (Html ())
index sessionId = runSessionEff sessionId do
  display <- Session.get (.display)
  case display of
    NoDisplay -> pure Template.bootstrap
    Desktop   -> Server.Desktop.index sessionId
    Mobile    -> Server.Mobile.index sessionId


view :: IsFilehub es => SessionId -> Eff es (Html ())
view sessionId = runSessionEff sessionId do
  display <- Session.get (.display)
  case display of
    Desktop   -> Server.Desktop.view sessionId
    Mobile    -> Server.Mobile.view sessionId
    NoDisplay -> Server.Mobile.view sessionId


controlPanel :: IsFilehub es => SessionId -> Eff es (Html ())
controlPanel sessionId = runSessionEff sessionId do
  display <- Session.get (.display)
  ctx <- makeTemplateContext sessionId
  pure $
    case display of
      Desktop -> runTemplate ctx Template.Desktop.controlPanel
      Mobile  -> runTemplate ctx Template.Mobile.controlPanel
      _       -> runTemplate ctx Template.Mobile.controlPanel


sideBar :: IsFilehub es => SessionId -> Eff es (Html ())
sideBar sessionId = runSessionEff sessionId do
  display <- Session.get (.display)
  case display of
    Desktop -> Server.Desktop.sideBar sessionId
    _       -> Server.Mobile.sideBar sessionId


toolBar :: IsFilehub es => SessionId -> Eff es (Html ())
toolBar sessionId = runSessionEff sessionId do
  display <- Session.get (.display)
  case display of
    Desktop -> Server.Desktop.toolBar sessionId
    _       -> Server.Mobile.toolBar sessionId


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
