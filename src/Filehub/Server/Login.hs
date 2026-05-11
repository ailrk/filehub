{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Login where

import Data.ByteString.Char8 qualified as ByteString
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID qualified as UUID
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Auth.Simple qualified as Auth.Simple
import Filehub.Auth.Types (AuthId(..))
import Filehub.Auth.Types.OIDC (AuthUrl (..), SomeOIDCFlow (..), OIDCFlow (..))
import Filehub.Cookie qualified as Cookies
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin)
import Filehub.Locale (Locale (..))
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Server.Util (parseHeader')
import Filehub.Session (SessionGet(..), SessionId(..))
import Filehub.Session qualified as Session
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Types (SessionSet(..))
import Filehub.Template (runTemplate, TemplateContext(..), makeTemplateContext)
import Filehub.Template.Login qualified as Template.Login
import Filehub.Theme qualified as Theme
import Filehub.Types (LoginForm(..) , FilehubEvent (..))
import Log (logInfo_, logAttention_)
import Lucid hiding (for_)
import Network.HTTP.Types.Header (hLocation)
import Network.URI qualified as URI
import Prelude hiding (init, readFile)
import Servant (Header , Headers , NoContent (..) , addHeader , err301 , err303    , errHeaders , noHeader  )
import UnliftIO (throwIO)
import Web.Cookie (SetCookie (..), defaultSetCookie)


-- | Return the login page
loginPage :: SessionId -> Maybe Text -> Maybe Text -> Filehub (Html ())
loginPage sessionId cookie Nothing = do
  ctx@TemplateContext { noLogin } <- makeTemplateContext sessionId
  if noLogin
     then go
     else do
       case fmap Text.encodeUtf8 cookie >>= parseHeader' >>= Cookies.fromCookies of
         Just authId' -> do
           authId <- Session.get sessionId (.authId)
           if authId == Just authId'
              then go
              else pure $ runTemplate ctx Template.Login.login
         Nothing -> pure $ runTemplate ctx Template.Login.login
  where
    go = throwIO do HTTPError (err301 { errHeaders = [(hLocation, "/")] })
loginPage sessionId _ (Just _) = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Login.login


loginToggleTheme :: SessionId -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
loginToggleTheme sessionId = do
  theme <- Session.get sessionId (.theme)
  case theme of
    Theme.Light -> Session.set sessionId (.theme) Theme.Dark
    Theme.Dark  -> Session.set sessionId (.theme) Theme.Light
  ctx <- makeTemplateContext sessionId
  let html = runTemplate ctx Template.Login.login'
  pure $ addHeader ThemeChanged html


loginChangeLocale :: SessionId -> Maybe Locale -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
loginChangeLocale _ Nothing = throwIO (FilehubError LocaleError "Invalid locale")
loginChangeLocale sessionId (Just locale) = do
  Session.set sessionId (.locale) locale
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
  Session.set sessionId (.oidcFlow) (Just (SomeOIDCFlow stage))
  case stage of
    AuthRequestPrepared _ _ _ _ (AuthUrl url) ->
      throwIO do
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
  Session.get sessionId (.oidcFlow) >>= \case
    Just (SomeOIDCFlow (stage@AuthRequestPrepared {})) -> do
        Auth.OIDC.callback stage code state
          >>= Auth.OIDC.exchangeToken
          >>= Auth.OIDC.verifyToken
          >>= Auth.OIDC.authenticateSession sessionId
          >>= Session.set sessionId (.oidcFlow) . Just . SomeOIDCFlow
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
      throwIO do
        HTTPError err303
          { errHeaders = [( "Location" , "/"), ("Set-Cookie", Cookies.renderSetCookie setCookie)]
          }
    Nothing -> do
      throwIO do
        HTTPError err303
          { errHeaders = [( "Location" , "/login")]
          }


loginAuthOIDCCallback _ _ _ mErr mErrDescription _ _ = do
  let message = fromMaybe "" mErr <> ", " <> fromMaybe "" mErrDescription
  throwIO do
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
      Session.set sessionId (.authId) Nothing
      Session.set sessionId (.oidcFlow) Nothing
      ActiveUser.Pool.delete authId
      addHeader
        (setCookie
          { setCookieExpires = Just (UTCTime (fromGregorian 1970 1 1) 0) })
        . addHeader "/login"
        <$> pure NoContent
    Nothing -> noHeader . noHeader <$> pure NoContent
