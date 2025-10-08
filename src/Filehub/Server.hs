{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Monad (when, forM, replicateM, void, join)
import Data.Aeson (object, KeyValue (..), (.:), withObject, Value)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.ClientPath (ClientPath (..))
import Data.ClientPath qualified as ClientPath
import Data.File (FileType(..), File(..), FileInfo, defaultFileWithContent, FileContent (..), withContent)
import Data.FileEmbed qualified as FileEmbed
import Data.Foldable (forM_)
import Data.Function (fix, (&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Effectful ( withRunInIO, MonadIO (liftIO), raise )
import Effectful.Concurrent.Async (async)
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logInfo_, logAttention_)
import Effectful.Reader.Dynamic (asks)
import Effectful.State.Static.Local (modify, get, evalState, execState)
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.OIDC (AuthUrl (..), SomeOIDCFlow (..))
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Auth.Simple qualified as Auth.Simple
import Filehub.Cookie qualified as Cookie
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env(..))
import Filehub.Error ( withServerError, FilehubError(..), withServerError, Error' (..) )
import Filehub.Locale (Locale)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Routes (Api (..))
import Filehub.Routes qualified as Routes
import Filehub.Server.Handler (ConfirmLogin, ConfirmReadOnly, ConfirmDesktopOnly)
import Filehub.Server.Handler qualified as Server.Handler
import Filehub.Server.Internal (withQueryParam, parseHeader', makeTemplateContext)
import Filehub.Server.Internal qualified as Server.Internal
import Filehub.Server.Platform.Desktop qualified as Server.Desktop
import Filehub.Server.Platform.Mobile qualified as Server.Mobile
import Filehub.Session (SessionId(..), TargetView (..))
import Filehub.Session qualified as Session
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Selected qualified as Selected
import Filehub.Sort qualified as Sort
import Filehub.Template qualified as Template
import Filehub.Template.Internal (runTemplate, TemplateContext(..))
import Filehub.Template.Login qualified as Template.Login
import Filehub.Template.Platform.Desktop qualified as Template.Desktop
import Filehub.Template.Platform.Mobile qualified as Template.Mobile
import Filehub.Template.Shared qualified as Template
import Filehub.Theme qualified as Theme
import Filehub.Types (Display (..), Layout (..), Resource (..), CopyState (..), TargetSessionData (..))
import Filehub.Types (FilehubEvent (..), LoginForm(..), MoveFile (..), UIComponent (..), SearchWord, OpenTarget, Resolution)
import Filehub.Types (UpdatedFile(..), NewFile(..), NewFolder(..), SortFileBy(..), UpdatedFile(..), Theme(..), Selected (..))
import Lens.Micro ((.~), (<&>))
import Lucid ( Html, class_, Term(term), With(with) )
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
import Servant (addHeader, err500, err303)
import Servant (errBody, Headers, Header, NoContent (..), err404, errHeaders, err301, noHeader, err400)
import Servant (serveWithContextT, Context (..), Application)
import Servant.API.EventStream (RecommendedEventSourceHeaders, recommendedEventSourceHeaders)
import Servant.Multipart (MultipartData, Mem)
import Servant.Server.Generic (AsServerT)
import System.Directory (removeFile)
import System.FilePath (takeFileName, (</>), takeDirectory, makeRelative)
import System.IO.Temp qualified as Temp
import System.Random (randomRIO)
import Target.Types (TargetId)
import Target.Types qualified as Target
import Text.Printf (printf)
import UnliftIO.Exception (SomeException, catch)
import UnliftIO.STM (readTBQueue, atomically, modifyTVar', readTVar, isEmptyTBQueue)
import Web.Cookie (SetCookie (..))
import Worker.Task (newTaskId)


#ifdef DEBUG
import Effectful ( MonadIO (liftIO) )
import System.FilePath ((</>))
import Paths_filehub qualified
import System.Directory (makeAbsolute)
import Data.ByteString (readFile)
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
  { init                  = init
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
  , themeCss              = themeCss
  , toggleTheme           = toggleTheme
  , changeLocale          = changeLocale
  , serve                 = serve
  , thumbnail             = thumbnail
  , manifest              = manifest
  , favicon               = pure $(FileEmbed.embedFile "data/filehub/favicon.ico")
  , static                = static
  , offline               = pure Template.offline
  , healthz               = pure "ok"
#ifdef DEBUG
  , debug1                = \_ -> pure $ addHeader (Dummy "Hello") NoContent
#endif
  }


init :: SessionId -> Resolution -> Filehub (Html ())
init sessionId res = do
  Session.Pool.update sessionId \s -> s & #resolution .~ Just res
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
  display <- Session.getDisplay sessionId & withServerError
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
      throwError (err400 { errBody = [i|Invalid ui component|]})


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
  notifications <- Session.getSessionNotifications sessionId & withServerError
  pendingTasks  <- Session.getPendingTasks sessionId         & withServerError
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
       case fmap Text.encodeUtf8 cookie >>= parseHeader' >>= Cookies.getAuthId of
         Just authId' -> do
           authId <- Session.getAuthId sessionId & withServerError
           if authId == Just authId'
              then go
              else pure $ runTemplate ctx Template.Login.login
         Nothing -> pure $ runTemplate ctx Template.Login.login
  where
    go = throwError (err301 { errHeaders = [(hLocation, "/")] })
loginPage sessionId _ (Just _) = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Login.login


loginToggleTheme :: SessionId -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
loginToggleTheme sessionId = do
  theme <- Session.getSessionTheme sessionId & withServerError
  case theme of
    Theme.Light -> Session.setSessionTheme sessionId Theme.Dark
    Theme.Dark  -> Session.setSessionTheme sessionId Theme.Light
  ctx <- makeTemplateContext sessionId
  let html = runTemplate ctx Template.Login.login'
  pure $ addHeader ThemeChanged html


loginChangeLocale :: SessionId -> Maybe Locale -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
loginChangeLocale _ Nothing = withServerError $ throwError (FilehubError LocaleError "Invalid locale")
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
  mSession <- Auth.Simple.authenticateSession sessionId form & withServerError
  case mSession of
    Just session -> do
      case Cookie.setAuthId session of
        Just setCookie -> do
          logInfo_ [i|User #{username} logged in|]
          addHeader setCookie . addHeader "/" <$> pure mempty
        Nothing -> do
          noHeader . noHeader <$> (pure failed)
    Nothing -> do
      noHeader . noHeader <$> (pure failed)


loginAuthOIDCRedirect :: SessionId -> Text -> Filehub NoContent
loginAuthOIDCRedirect sessionId providerName = do
  stage <- withServerError do
    Auth.OIDC.init providerName >>= Auth.OIDC.authorize
  Auth.OIDC.setSessionOIDCFlow sessionId (Just stage)
  case stage of
    Auth.OIDC.AuthRequestPrepared _ _ _ _ (AuthUrl url) ->
      throwError err303
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
  withServerError do
    Auth.OIDC.getSessionOIDCFlow sessionId >>= \case
      Just (SomeOIDCFlow (stage@Auth.OIDC.AuthRequestPrepared {})) -> do
          Auth.OIDC.callback stage code state
            >>= Auth.OIDC.exchangeToken
            >>= Auth.OIDC.verifyToken
            >>= Auth.OIDC.authenticateSession sessionId
            >>= Auth.OIDC.setSessionOIDCFlow sessionId . Just
      _ -> do
        logAttention_ "OIDC Error: invalid stage"
        pure ()
  session <- Session.Pool.get sessionId & withServerError
  case Cookie.setAuthId session of
    Just setCookie -> do
      throwError err303
        { errHeaders = [( "Location" , "/"), ("Set-Cookie", Cookies.renderSetCookie setCookie)]
        }
    Nothing ->
      throwError err303
        { errHeaders = [( "Location" , "/login")]
        }
loginAuthOIDCCallback _ _ _ mErr mErrDescription _ _ = do
  let message = fromMaybe "" mErr <> ", " <> fromMaybe "" mErrDescription
  throwError err303
    { errHeaders = [( "Location" , "/login?error=\"" <> Text.encodeUtf8 message <> "\"" )]
    }


logout :: SessionId -> ConfirmLogin -> Filehub (Headers '[ Header "Set-Cookie" SetCookie
                                                         , Header "HX-Redirect" Text
                                                         ] NoContent)
logout sessionId _ = withServerError do
  session <- Session.Pool.get sessionId
  case (,) <$> Cookie.setAuthId session <*> session.authId of
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
  withServerError do
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
  withServerError do
    storage <- Session.getStorage sessionId
    storage.new (Text.unpack path)
  view sessionId


updateFile :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> UpdatedFile -> Filehub (Html ())
updateFile sessionId _ _ (UpdatedFile clientPath content) = do
  let path = clientPath.unClientPath
  withServerError do
    storage <- Session.getStorage sessionId
    storage.write path $ defaultFileWithContent
      { path     = path
      , content  = FileContentRaw (Text.encodeUtf8 content)
      }
  view sessionId


newFolder :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFolder -> Filehub (Html ())
newFolder = \sessionId _ _ (NewFolder path) -> do
  withServerError do
    storage <- Session.getStorage sessionId
    storage.newFolder (Text.unpack path)
  view sessionId


delete :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> [ClientPath] -> Bool
       -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                            , Header "HX-Trigger" FilehubEvent
                            ] (Html ()))
delete sessionId _ _ clientPaths deleteSelected = do
  count <- Selected.countSelected sessionId & withServerError
  void $ async do
    taskId <- newTaskId
    withServerError do
      storage <- Session.getStorage sessionId
      root    <- Session.getRoot sessionId
      Session.notify sessionId (DeleteProgressed taskId 0)

      nDeleted <- execState @Integer 0 do
        forM_ clientPaths \clientPath -> do
          modify @Integer (+ 1)
          n <- get @Integer
          let path = ClientPath.fromClientPath root clientPath
          raise $ storage.delete path
          Session.notify sessionId (DeleteProgressed taskId (n % max 1 (fromIntegral count)))

      when deleteSelected do
        allSelecteds <- Selected.allSelecteds sessionId
        evalState @Integer nDeleted do
          forM_ allSelecteds \(target, selected) -> do
            Session.withTarget sessionId (Target.getTargetId target) \_ _ -> do
              case selected of
                NoSelection -> pure ()
                Selected x xs -> do
                  forM_ (fmap (ClientPath.fromClientPath root) (x:xs)) \path -> do
                    modify @Integer (+ 1)
                    n <- get @Integer
                    raise $ storage.delete path
                    Session.notify sessionId (DeleteProgressed taskId (n % max 1 (fromIntegral count)))
      Session.notify sessionId (DeleteProgressed taskId 1)
      Session.notify sessionId (TaskCompleted taskId)
  Server.Internal.clear sessionId
  addHeader count . addHeader SSEStarted <$> index sessionId


copy :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Filehub (Html ())
copy sessionId _ _ = do
  withServerError do
    Copy.select sessionId
    Copy.copy sessionId
  controlPanel sessionId


copy1 :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Maybe ClientPath -> Filehub (Html ())
copy1 sessionId _ _ mClientPath = do
  clientPath <- withQueryParam mClientPath
  Server.Internal.clear sessionId
  Selected.setSelected sessionId (Selected clientPath [])
  withServerError do
    Copy.select sessionId
    Copy.copy sessionId
  index sessionId


paste :: SessionId -> ConfirmLogin -> ConfirmReadOnly
      -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                           , Header "HX-Trigger" FilehubEvent
                           ] (Html ()))
paste sessionId _ _ = do
  count <- Selected.countSelected sessionId & withServerError
  taskId <- newTaskId
  withServerError do
    state <- Copy.getCopyState sessionId
    case state of
      Paste selections -> do
        void $ async do
          TargetView to sessionData _ <- Session.currentTarget sessionId
          Session.notify sessionId (PasteProgressed taskId 0)
          evalState @Integer 0 do
            forM_ selections \(from, files) -> do
              forM_ files \f -> do
                let go currentDir file = do -- expose the recursion with the fix point
                      case file.content of
                        Regular -> do
                          conduit <- Session.withTarget sessionId (Target.getTargetId from) \_ storage -> do
                            storage.readStream file
                          Session.withTarget sessionId (Target.getTargetId to) \_ storage -> do
                            storage.write (currentDir </> takeFileName file.path) (file `withContent` (FileContentConduit conduit))
                        Dir -> do -- copy directory layer by layer
                          dst <- Session.withTarget sessionId (Target.getTargetId to) \_ storage -> do
                            let dst = currentDir </> takeFileName file.path
                            storage.newFolder dst
                            pure dst
                          Session.withTarget sessionId (Target.getTargetId from)
                            \(TargetView _ (TargetSessionData { currentDir = savedDir }) _) storage -> do
                              storage.cd file.path
                              do
                                dirFiles <- storage.lsCwd
                                forM_ dirFiles (go dst)
                              storage.cd savedDir -- go back
                go sessionData.currentDir f
                modify @Integer (+ 1)
                n <- get @Integer
                Session.notify sessionId (PasteProgressed taskId (n % max 1 (fromIntegral count)))
          Copy.setCopyState sessionId NoCopyPaste
          Selected.clearSelectedAllTargets sessionId
          Session.notify sessionId (PasteProgressed taskId 1)
          Session.notify sessionId (TaskCompleted taskId)
        pure ()
      _ -> do
        logAttention_ [i|Paste error: #{sessionId}, not in pastable state.|]
        throwError (FilehubError SelectError "Not in a pastable state")
  Server.Internal.clear sessionId
  addHeader count . addHeader SSEStarted <$> index sessionId


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
  display <- Session.getDisplay sessionId & withServerError
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
  display <- Session.getDisplay sessionId & withServerError
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
  withServerError do
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
      count         <- Selected.countSelected sessionId & withServerError
      sideBar'      <- sideBar sessionId
      controlPanel' <- controlPanel sessionId
      pure $ addHeader count do
        sideBar' `with` [ term "hx-swap-oob" "true" ]
        controlPanel'


upload :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> MultipartData Mem -> Filehub (Html ())
upload sessionId _ _ multipart = do
  void $ async do
    taskId <- newTaskId
    withServerError do
      Session.notify sessionId (UploadProgressed taskId 0)
      storage <- Session.getStorage sessionId
      storage.upload multipart
      Session.notify sessionId (UploadProgressed taskId 1)
      Session.notify sessionId (TaskCompleted taskId)
  index sessionId


download :: SessionId -> ConfirmLogin -> [ClientPath]
         -> Filehub (Headers '[ Header "Content-Disposition" String ] (ConduitT () ByteString (ResourceT IO) ()))
download sessionId _ clientPaths = do
  storage <- Session.getStorage sessionId & withServerError
  root    <- Session.getRoot sessionId & withServerError
  case clientPaths of
    [clientPath@(ClientPath path)] -> do
      file    <- storage.get (ClientPath.fromClientPath root clientPath) & withServerError
      conduit <- withServerError (storage.download clientPath)
      let filename =
            case file.content of
              Regular -> printf "attachement; filename=%s" (takeFileName path)
              Dir     -> printf "attachement; filename=%s.zip" (takeFileName path)
      pure $ addHeader filename conduit
    _ -> do
      (zipPath, _) <- liftIO do
        tempDir <- Temp.getCanonicalTemporaryDirectory
        Temp.openTempFile tempDir "DXXXXXX.zip"
      tasks <- do
        forM (fmap (ClientPath.fromClientPath root) clientPaths) \path -> withServerError do
          file    <- storage.get path
          conduit <- storage.readStream file
          pure (path, conduit)
      Zip.createArchive zipPath do
        forM_ tasks \(path, conduit) -> do
          m <- Zip.mkEntrySelector  (makeRelative root path)
          Zip.sinkEntry Zip.Zstd conduit m
      tag <- Text.pack <$> replicateM 8 (randomRIO ('a', 'z'))
      let conduit =
            Conduit.bracketP
              (pure ())
              (\_ -> liftIO $ removeFile zipPath)
              (\_ -> Conduit.sourceFile zipPath)
      pure $ addHeader (printf "attachement; filename=%s.zip" tag) conduit


move :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> MoveFile
     -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent
                          , Header "HX-Trigger" FilehubEvent
                          ] (Html ()))
move sessionId _ _ (MoveFile src tgt) = do
  root         <- Session.getRoot sessionId & withServerError
  storage      <- Session.getStorage sessionId & withServerError
  let srcPaths =  fmap (ClientPath.fromClientPath root) src
  let tgtPath  =  ClientPath.fromClientPath root tgt
  void $ async do
    taskId <- newTaskId
    -- check before take action
    forM_ srcPaths \srcPath -> withServerError do
      isTgtDir <- storage.isDirectory tgtPath
      when (not isTgtDir) do
        throwError (FilehubError InvalidDir "Target is not a directory")

      when (srcPath == tgtPath)  do
        throwError (FilehubError InvalidDir "Can't move to the same directory")

      when (takeDirectory srcPath == tgtPath)  do
        throwError (FilehubError InvalidDir "Already in the current directory")

    let mvPairs = fmap (\srcPath -> (srcPath, tgtPath </> (takeFileName srcPath))) srcPaths

    withServerError do
      Session.notify sessionId (MoveProgressed taskId 0)
      storage.mv mvPairs
      Session.notify sessionId (MoveProgressed taskId 1)
      Session.notify sessionId (TaskCompleted taskId)

  Server.Internal.clear sessionId
  addHeader FileMoved . addHeader SSEStarted <$> index sessionId


contextMenu :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> [ClientPath] -> Filehub (Html ())
contextMenu sessionId _ _ paths = Server.Desktop.contextMenu sessionId paths


cancel :: SessionId -> ConfirmLogin -> Filehub (Headers '[Header "X-Filehub-Selected-Count" Int] (Html ()))
cancel sessionId _ = do
  Server.Internal.clear sessionId
  count <- Selected.countSelected sessionId & withServerError
  addHeader count <$> index sessionId


initViewer :: SessionId -> ConfirmLogin -> Maybe ClientPath
           -> Filehub (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
initViewer sessionId _ mClientPath = do
  withServerError do
    clientPath <- withQueryParam mClientPath
    root       <- Session.getRoot sessionId
    payload    <- initViewer' root clientPath
    pure $ addHeader payload NoContent
  where
    initViewer' root clientPath = do
      storage <- Session.getStorage sessionId
      let filePath  =  ClientPath.fromClientPath root clientPath
      let dir       =  takeDirectory filePath
      order         <- Session.getSortFileBy sessionId
      files         <- takeResourceFiles . Sort.sortFiles order <$> (storage.ls dir)
      let idx       =  fromMaybe 0 $ List.elemIndex filePath (fmap (.path) files)
      let resources =  fmap (toResource root) files
      pure $ ViewerInited resources idx

    isResource :: MimeType -> Bool
    isResource s = any (s `isMime`)  ["image", "video", "audio"]

    takeResourceFiles :: [FileInfo] -> [FileInfo]
    takeResourceFiles = filter (isResource . (.mimetype))

    toResource :: FilePath -> FileInfo -> Resource
    toResource root f =
      Resource
        { url = let ClientPath path = ClientPath.toClientPath root f.path -- encode path url
                 in ClientPath.RawClientPath [i|/serve?file=#{path}|]
                                                  , mimetype = Text.decodeUtf8 f.mimetype
        }


open :: SessionId -> ConfirmLogin -> Maybe OpenTarget -> Maybe ClientPath
     -> Filehub (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
open _ _ mTarget mClientPath = do
  clientPath <- withQueryParam mClientPath
  target     <- withQueryParam mTarget
  pure $ addHeader (Opened target clientPath) NoContent


changeTarget :: SessionId -> ConfirmLogin -> Maybe TargetId
             -> Filehub (Headers '[Header "HX-Trigger-After-Swap" FilehubEvent] (Html ()))
changeTarget sessionId _ mTargetId = do
  savedTargetId <- withServerError do
    TargetView saved _ _ <- Session.currentTarget sessionId
    pure $ Target.getTargetId saved

  let restore = Session.changeCurrentTarget sessionId savedTargetId & withServerError
  targetId <- withQueryParam mTargetId
  Session.changeCurrentTarget sessionId targetId & withServerError

  html <- withRunInIO \unlift -> do
    unlift (index sessionId)
      `catch` \(_ :: SomeException) -> unlift do
        restore
        throwError (err500 { errBody = [i|Invalid target|]})
  pure $ addHeader TargetChanged html


themeCss :: SessionId -> Filehub ByteString
themeCss sessionId = do
  customThemeDark  <- (fmap . fmap) Theme.customTheme2Css (asks @Env (.customThemeDark))
  customThemeLight <- (fmap . fmap) Theme.customTheme2Css (asks @Env (.customThemeLight))
#ifdef DEBUG
  theme <- Session.getSessionTheme sessionId & withServerError
  dir <- liftIO $ Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data/filehub")
  case theme of
    Dark  -> liftIO $ maybe (readFile (dir </> "theme-dark.css")) pure customThemeDark
    Light -> liftIO $ maybe (readFile (dir </> "theme-light.css")) pure customThemeLight
#else
  theme <- Session.getSessionTheme sessionId & withServerError
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
  theme <- Session.getSessionTheme sessionId & withServerError
  case theme of
    Theme.Light -> Session.setSessionTheme sessionId Theme.Dark
    Theme.Dark  -> Session.setSessionTheme sessionId Theme.Light
  html <- index sessionId
  pure $ addHeader ThemeChanged (html `with` [ class_ "fade-in " ])


changeLocale :: SessionId -> Maybe Locale -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
changeLocale _ Nothing = withServerError $ throwError (FilehubError LocaleError "Invalid locale")
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
  withServerError do
    storage    <- Session.getStorage sessionId
    root       <- Session.getRoot sessionId
    clientPath <- withQueryParam mFile
    let path   = ClientPath.fromClientPath root clientPath
    file       <- storage.get path
    conduit    <- storage.readStream file
    pure
      . addHeader (ByteString.unpack file.mimetype)
      . addHeader (printf "inline; filename=%s" (takeFileName path))
      . addHeader "public, max-age=31536000, immutable"
      $ conduit


thumbnail :: SessionId -> ConfirmLogin -> Maybe ClientPath
          -> Filehub (Headers '[ Header "Content-Type" String
                               , Header "Content-Disposition" String
                               , Header "Cache-Control" String
                               ]
                               (ConduitT () ByteString (ResourceT IO) ()))
thumbnail sessionId _ mFile = do
  withServerError do
    storage    <- Session.getStorage sessionId
    root       <- Session.getRoot sessionId
    clientPath <- withQueryParam mFile
    let path   = ClientPath.fromClientPath root clientPath
    file       <- storage.get path
    conduit    <- serveOriginal storage file
    pure
      . addHeader (ByteString.unpack file.mimetype)
      . addHeader (printf "inline; filename=%s" (takeFileName path))
      . addHeader "public, max-age=31536000, immutable"
      $ conduit
  where
    serveOriginal storage file =
      if
        | file.mimetype `isMime` "image" -> storage.readStream file
        | otherwise                      -> throwError (FilehubError FormatError "Invalid mime type for thumbnail") & withServerError



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
    Nothing -> throwError (err404 { errBody = [i|File doesn't exist|]})
#endif


------------------------------------
-- Components
------------------------------------


index :: SessionId -> Filehub (Html ())
index sessionId = do
  display <- Session.getDisplay sessionId & withServerError
  case display of
    NoDisplay -> pure Template.bootstrap
    Desktop   -> Server.Desktop.index sessionId
    Mobile    -> Server.Mobile.index sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  display <- Session.getDisplay sessionId & withServerError
  case display of
    Desktop   -> Server.Desktop.view sessionId
    Mobile    -> Server.Mobile.view sessionId
    NoDisplay -> Server.Mobile.view sessionId


controlPanel :: SessionId -> Filehub (Html ())
controlPanel sessionId = do
  ctx <- makeTemplateContext sessionId
  withServerError do
    display <- Session.getDisplay sessionId
    pure $
      case display of
        Desktop -> runTemplate ctx Template.Desktop.controlPanel
        Mobile  -> runTemplate ctx Template.Mobile.controlPanel
        _       -> runTemplate ctx Template.Mobile.controlPanel


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  display <- Session.getDisplay sessionId & withServerError
  case display of
    Desktop -> Server.Desktop.sideBar sessionId
    _       -> Server.Mobile.sideBar sessionId


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  display <- Session.getDisplay sessionId & withServerError
  case display of
    Desktop -> Server.Desktop.toolBar sessionId
    _       -> Server.Mobile.toolBar sessionId


------------------------------------
-- application
------------------------------------


application :: Env -> Application
application env
  = Wai.Middleware.gzip Wai.Middleware.defaultGzipSettings
  . Wai.Middleware.logStdout
  . Wai.Middleware.stripCookiesForStatic
  . Wai.Middleware.exposeHeaders
  . Wai.Middleware.sessionMiddleware env
  . Wai.Middleware.dedupHeadersKeepLast
  . Wai.Middleware.displayMiddleware env
  . serveWithContextT Routes.api ctx (Server.Handler.toServantHandler env)
  $ server
  where
    ctx = Server.Handler.sessionHandler env
        :. Server.Handler.readOnlyHandler env
        :. Server.Handler.desktopOnlyHandler env
        :. Server.Handler.mobileOnlyHandler env
        :. Server.Handler.loginHandler env
        :. EmptyContext
