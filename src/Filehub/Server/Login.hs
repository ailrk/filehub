{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Login where

import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
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
import Filehub.Session (SessionId(..), Session(..))
import Filehub.Session.Pool qualified as Session.Pool
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
import UnliftIO (throwIO, atomically)
import Web.Cookie (SetCookie (..), defaultSetCookie)
import Filehub.Session.Pool (withSession, modifySession)


-- | Return the login page
loginPage :: SessionId -> Maybe Text -> Maybe Text -> Filehub (Html ())
loginPage sessionId cookie Nothing = do
  ctx@TemplateContext { noLogin } <- makeTemplateContext sessionId
  if noLogin
     then go
     else do
       case fmap T.encodeUtf8 cookie >>= parseHeader' >>= Cookies.fromCookies of
         Just authId' -> do
           authId <- withSession sessionId (pure . (.authId))
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
  theme <- withSession sessionId (pure . (.theme))
  case theme of
    Theme.Light -> modifySession sessionId \s -> pure $ s { theme = Theme.Dark }
    Theme.Dark  -> modifySession sessionId \s -> pure $ s { theme = Theme.Light }
  ctx <- makeTemplateContext sessionId
  let html = runTemplate ctx Template.Login.login'
  pure $ addHeader ThemeChanged html


loginChangeLocale :: SessionId -> Maybe Locale -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
loginChangeLocale _ Nothing = throwIO (FilehubError LocaleError "Invalid locale")
loginChangeLocale sessionId (Just locale) = do
  modifySession sessionId \s -> pure $ s { locale = locale }
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
  modifySession sessionId \s -> pure $ s { oidcFlow = (Just (SomeOIDCFlow stage)) }
  case stage of
    AuthRequestPrepared _ _ _ _ (AuthUrl url) ->
      throwIO do
        HTTPError err303
          { errHeaders =
              [( "Location"
               , BC.pack (URI.uriToString id url "")
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

  withSession sessionId (pure . (.oidcFlow)) >>= \case
    Just (SomeOIDCFlow (stage@AuthRequestPrepared {})) -> do
      stg <- Auth.OIDC.callback stage code state
        >>= Auth.OIDC.exchangeToken
        >>= Auth.OIDC.verifyToken
        >>= Auth.OIDC.authenticateSession sessionId
      modifySession sessionId \s -> pure $ s { oidcFlow = Just (SomeOIDCFlow stg)}
    _ -> do
      logAttention_ "[s9vf9d] OIDC Error: invalid stage"
      pure ()
  session <- Session.Pool.get sessionId >>= atomically
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
      { errHeaders = [( "Location" , "/login?error=\"" <> T.encodeUtf8 message <> "\"" )]
      }


logout :: SessionId -> ConfirmLogin -> Filehub (Headers '[ Header "Set-Cookie" SetCookie
                                                         , Header "HX-Redirect" Text
                                                         ] NoContent)
logout sessionId _ = do
  session <- Session.Pool.get sessionId >>= atomically
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
      modifySession sessionId \s -> pure do
        s { authId = Nothing
          , oidcFlow = Nothing
          }
      ActiveUser.Pool.delete authId
      addHeader
        (setCookie
          { setCookieExpires = Just (UTCTime (fromGregorian 1970 1 1) 0) })
        . addHeader "/login"
        <$> pure NoContent
    Nothing -> noHeader . noHeader <$> pure NoContent
