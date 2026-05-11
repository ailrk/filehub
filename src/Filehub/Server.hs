{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- [Debug Note]
-- When using Debug.Breakpoint we need to compile with the plugin on all
-- modules that spawns threads. It's too slow to add it to every modules.
-- Luckily, most threads are created here by servant handlers.
#ifdef DEBUG
{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}
#endif

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
import Control.Monad.Reader (asks)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (object, KeyValue (..), (.:), withObject, Value)
import Data.Aeson.Types (parseMaybe)
import Data.Binary.Builder qualified as Builder
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
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import Data.UUID qualified as UUID
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.Types.OIDC (AuthUrl (..), SomeOIDCFlow (..))
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
import Filehub.Server.File qualified
import Filehub.Server.Login qualified
import Filehub.Server.Notification qualified
import Filehub.Server.Search qualified
import Filehub.Server.SharedLink qualified
import Filehub.Server.Static qualified
import Filehub.Server.UI (index, clear, controlPanel, sideBar, view)
import Filehub.Server.UI qualified
import Filehub.Server.UI.Desktop qualified as Server.Desktop
import Filehub.Server.UI.Mobile qualified as Server.Mobile
import Filehub.Server.Util (withQueryParam, parseHeader')
import Filehub.Session (SessionGet(..))
import Filehub.Session (SessionId(..), TargetView (..))
import Filehub.Session qualified as Session
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
import Foreign.C (CTime(..))
import Log (logInfo_, logAttention_, logAttention)
import Lucid (Html)
import Lucid hiding (for_)
import Network.HTTP.Types (status404)
import Network.HTTP.Types.Header (hLocation)
import Network.Mime (MimeType)
import Network.Mime qualified as Mime
import Network.Mime.Extended (isMime)
import Network.URI qualified as URI
import Network.Wai (Request(..), responseLBS, responseFile, responseStream)
import Network.Wai.Application.Static (defaultFileServerSettings, StaticSettings (..), staticApp)
import Network.Wai.Middleware.Extended qualified as Wai.Middleware
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
import UnliftIO (throwIO)
import UnliftIO.Async (async, forConcurrently_)
import UnliftIO.Exception (SomeException, catch)
import UnliftIO.STM (readTBQueue, atomically, isEmptyTBQueue, modifyTVar', readTVar, newTVarIO, writeTBQueue)
import WaiAppStatic.Types (MaxAge(..), toPiece, LookupResult (..), File(..), unsafeToPiece)
import Web.Cookie (SetCookie (..), defaultSetCookie)
import Worker.Task (TaskId, newTaskId)
import Control.Monad (when)
import Data.String.Interpolate (i)
import Data.UUID qualified as UUID
import Filehub.Cookie qualified as Cookies
import Filehub.Env (Env (..))
import Filehub.Monad ( toIO )
import Filehub.Server.Util (parseHeader')
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Types (Session(..), SessionId(..))
import Filehub.UserAgent qualified as UserAgent
import Log (logTrace_)
import Network.HTTP.Types (hUserAgent, status500)
import Network.HTTP.Types.Header (hSetCookie)
import Network.Wai
import Prelude hiding (readFile)
import Web.Cookie (defaultSetCookie, SetCookie (..))
import Data.ByteString.Char8 qualified as Char8
import Filehub.Session qualified as Session
import UnliftIO (MonadIO(..), try, throwIO)
import Filehub.Error (FilehubError (..), Error' (..))


------------------------------------
-- Sever endpoints
------------------------------------


server :: Env -> Api (AsServerT Filehub)
server env = Api
  { initialize            = initialize
  , home                  = home
  , refresh               = refresh
  , listen                = Filehub.Server.Notification.listen
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
  , search                = Filehub.Server.Search.search
  , sortTable             = Filehub.Server.UI.sortTable
  , selectLayout          = Filehub.Server.UI.selectLayout
  , selectRows            = Filehub.Server.UI.selectRows
  , upload                = Filehub.Server.File.upload
  , download              = Filehub.Server.File.download
  , copy                  = Filehub.Server.File.copy
  , copy1                 = Filehub.Server.File.copy1
  , paste                 = Filehub.Server.File.paste
  , move                  = Filehub.Server.File.move
  , cancel                = Filehub.Server.UI.cancel
  , contextMenu           = Filehub.Server.UI.contextMenu
  , initViewer            = Filehub.Server.UI.initViewer
  , open                  = Filehub.Server.UI.open
  , changeTarget          = changeTarget
  , shared                = Filehub.Server.SharedLink.shared
  , sharedAuth            = Filehub.Server.SharedLink.sharedAuth
  , themeCss              = Filehub.Server.Static.themeCss
  , toggleTheme           = Filehub.Server.UI.toggleTheme
  , changeLocale          = Filehub.Server.UI.changeLocale
  , serve                 = Filehub.Server.File.serve env
  , toggleSidebar         = Filehub.Server.UI.toggleSidebar
  , thumbnail             = Filehub.Server.File.thumbnail
  , manifest              = Filehub.Server.Static.manifest
  , favicon               = pure $(FileEmbed.embedFile "data/filehub/favicon.ico")
  , static                = Filehub.Server.Static.static
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
  Session.set sessionId (.resolution) (Just res)
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
  m <- Filehub.Server.Static.manifest
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



------------------------------------
-- Middleware
------------------------------------



displayMiddleware :: Env -> Middleware
displayMiddleware  env app req respond = toIO onErr env do
  let mCookie    = lookup "Cookie" (requestHeaders req)
  sessionId <- case mCookie >>= parseHeader' >>= Cookies.fromCookies of
                 Just sessionId -> pure sessionId
                 Nothing        -> throwIO (HTTPError (err400 { errBody = [i|Invalid session id|]}))

  -- session <- Session.Pool.get sessionId

  -- set device type
  do
    let mUserAgent = lookup hUserAgent (requestHeaders req)
        deviceType =
          case mUserAgent of
            Just userAgent -> UserAgent.detectDeviceType userAgent
            Nothing        -> UserAgent.Unknown

    deviceType' <- Session.get sessionId (.deviceType)
    when (deviceType' /= deviceType) do
      Session.set sessionId (.deviceType) deviceType
      -- Session.Pool.update sessionId (#deviceType .~ deviceType)

  -- set display cookie
  -- Note only the server set the cookie.
  setCookieHeader <- do
    currentDisplay <- Session.get sessionId (.display)
    let displaySetCookie = defaultSetCookie
          { setCookieName     = "display"
          , setCookieValue    = Char8.pack (show currentDisplay)
          , setCookieExpires  = Nothing
          , setCookieHttpOnly = False
          , setCookiePath     = Just "/"
          , setCookieSecure   = False
          }
    let header = ("Set-Cookie", Cookies.renderSetCookie displaySetCookie)
    pure (header :)

  liftIO $ app req \res ->
    let res' = mapResponseHeaders setCookieHeader res
     in respond res'

  where
    onErr _ = respond $ responseLBS (status500) [] "invalid display information"


-- | If session is not present, create a new session
sessionMiddleware :: Env -> Middleware
sessionMiddleware env app req respond = toIO onErr env do
  let mCookie    = lookup "Cookie" (requestHeaders req)
  let mSessionId = mCookie >>= parseHeader' >>= Cookies.fromCookies
  case mSessionId of
    Just sessionId -> do
      eSession <- try $ Session.Pool.get sessionId
      case eSession of
        Left (FilehubError InvalidSession _) -> respondWithNewSession
        Left err                             -> throwIO err
        Right _                              -> liftIO $ app req respond
    Nothing -> do
      logTrace_ [i|[0vz333] No session found.|]
      respondWithNewSession
  where
    respondWithNewSession = do
      session <- Session.Pool.newSession
      let sessionIdSetCookie = defaultSetCookie
            { setCookieName     = "sessionId"
            , setCookieValue    = let SessionId sid = session.sessionId in UUID.toASCIIBytes sid
            , setCookieExpires  = Just session.expireDate
            , setCookieHttpOnly = True
            , setCookiePath     = Just "/"
            , setCookieSecure   = False -- Since there is no authentication, Secure is set to False
            }
      let sessionId@(SessionId sid) = session.sessionId
      let setCookieHeader           = ("Set-Cookie", Cookies.renderSetCookie sessionIdSetCookie)
      let injectedCookieHeader      = ("Cookie", "sessionId=" <> UUID.toASCIIBytes sid)
      let req'                      = req { requestHeaders = injectedCookieHeader : requestHeaders req }
      logTrace_ [i|[vzs0e1] New session: #{sessionId}|]
      liftIO $ app req' \res ->
        let res' = mapResponseHeaders (setCookieHeader :) res
         in respond res'
    onErr _ = respond $ responseLBS status500 [] "server error"



-- | We want to strict all cookies on responds to static files, otherwise CDN will not cache these
--   content.
stripCookiesForStatic :: Middleware
stripCookiesForStatic app req respond
  | not (null path), "static":_ <- path = app req \res -> do respond $ mapResponseHeaders (filter (\(h,_) -> h /= hSetCookie)) res
  | otherwise = app req respond
  where path = pathInfo req


------------------------------------
-- Application
------------------------------------


application :: Env -> Application
application env
  = Wai.Middleware.gzip Wai.Middleware.defaultGzipSettings
  . (if env.enableWAILog then Wai.Middleware.logStdout else id)
  . stripCookiesForStatic
  . Wai.Middleware.exposeHeaders
  . sessionMiddleware env
  . Wai.Middleware.dedupHeadersKeepLast
  . displayMiddleware env
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
