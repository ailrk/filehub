{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template.Login (login, login', loginFailed) where

import Lucid
import Data.Text (Text)
import Filehub.Auth.OIDC (OIDCAuthProviders(..))
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Template (Template, TemplateContext(..))
import Control.Monad (when)
import Data.Foldable (forM_)
import Filehub.Locale (Phrase(..), phrase, Locale (..))
import Filehub.Links (linkToText, apiLinks)
import Filehub.Routes (Api(..))
import Filehub.Theme (Theme(..))
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (asks)
import Lucid.Htmx (HxPost(..), hxTarget, HxSwap (..), Swap (..), hxGet)


login :: Template (Html ())
login = do
  html <- login'
  pure do
    script_ [ src_ "/static/htmx2.0.3.js" ] ("" :: Text)
    script_ [ src_ "/static/login.js", type_ "module" ] ("" :: Text)
    meta_   [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover" ]
    link_   [ rel_ "stylesheet", href_ "/static/login.css" ]
    link_   [ rel_ "stylesheet", href_ "/static/boxicons2.1.4.css" ]
    link_   [ rel_ "icon", type_ "image/png", href_ "/favicon-96x96.png", sizes_ "96x96"]
    link_   [ rel_ "icon", type_ "image/svg+xml", href_ "/favicon.svg"]
    link_   [ rel_ "shortcut icon", href_ "/favicon.ico"]
    link_   [ rel_ "stylesheet", href_ "/theme.css" ]
    html


login' :: Template (Html ())
login' = do
  OIDCAuthProviders providers <- asks (.oidcAuthProviders)
  Phrase
    { login_button
    , login_or
    , login_password
    , login_username } <- phrase <$> asks (.locale)
  themeBtn' <- themeBtn
  pure do
    div_ [ id_ "login"  ] do
      div_ [ id_ "top-bar" ] do
        themeBtn'
        localeBtn

      div_ [ id_ "login-form" ] do
        form_ [ hxPost @Text "/login"
              , hxTarget "#login-error"
              , hxSwap OuterHTML
              , class_ "panel "
              , autocomplete_ "off"
              ] do
          div_ do
            input_ [ type_ "text"
                   , id_ "username"
                   , name_ "username"
                   , placeholder_ login_username
                   , autocomplete_ "new-password"
                   ]
          div_ do
            input_ [ type_ "password"
                   , id_ "password"
                   , name_ "password"
                   , placeholder_ login_password
                   , autocomplete_ "off"
                   ]
          div_ [ id_ "login-error"
               , hxTarget "this"
               , hxSwap OuterHTML
               ] mempty
          div_ do
            button_ (toHtml login_button)
        when (not (null providers)) do
          div_ [ class_ "or-sep "] (toHtml login_or)
          div_ [ class_ "panel oidc " ] do
            forM_ providers \provider -> do
              button_ [ onclick_ (mconcat ["window.location.href='", (linkToText (apiLinks.loginAuthOIDCRedirect provider.name)), "'"]) ]
                (toHtml provider.name)


localeBtn :: Html ()
localeBtn =
  div_ [ id_ "locale" ] do
    button_ [ class_ "btn btn-control " ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-world" ] mempty
    div_ [ class_ "dropdown-content " ] do
      let item :: Locale -> Html () -> Html ()
          item loc label = div_ [ class_ "dropdown-item"
                                , hxGet (apiLinks.loginChangeLocale (Just loc))
                                , hxTarget "#login"
                                , hxSwap OuterHTML
                                ] do span_ label
      item EN "English"
      item ZH_CN "简体中文"
      item ZH_TW "繁體中文"
      item ZH_HK "繁體中文"
      item JA "日本語"
      item ES "Español"
      item FR "Français"
      item DE "Deutsch"
      item KO "한국어"
      item RU "Русский"
      item PT "Português"
      item IT "Italiano"


themeBtn :: Template (Html ())
themeBtn = do
  theme <- asks (.theme)
  pure do
    case theme of
      Light -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , hxGet apiLinks.loginToggleTheme
                , hxTarget "#login"
                , hxSwap OuterHTML
                ] do
          i_ [ class_ "bx bxs-moon" ] mempty
      Dark -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , hxGet apiLinks.loginToggleTheme
                , hxTarget "#login"
                , hxSwap OuterHTML
                ] do
          i_ [ class_ "bx bxs-sun" ] mempty


loginFailed :: Maybe Text -> Template (Html ())
loginFailed mMsg = do
  Phrase
    { login_error } <- phrase <$> asks (.locale)
  pure do
    div_ [ id_ "login-error"
         , class_ "show fade-in "
         , hxTarget "this"
         , hxSwap OuterHTML
         ] do
      span_ (toHtml (fromMaybe login_error mMsg))
