{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template.Login where

import Lucid
import Data.Text (Text)
import Filehub.Auth.OIDC (OIDCAuthProviders(..))
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Template.Internal
import Control.Monad (when)
import Data.Foldable (forM_)
import Filehub.Locale (Phrase(..), phrase, Locale (..))
import Effectful.Reader.Dynamic (asks)
import Filehub.Links (linkToText, apiLinks)
import Filehub.Routes (Api(..))
import Filehub.Theme (Theme(..))
import Data.Maybe (fromMaybe)


login :: Template (Html ())
login = do
  html <- login'
  pure do
    script_ [ src_ "/static/htmx2.0.3.js" ] ("" :: Text)
    script_ [ src_ "/static/login.js", type_ "module" ] ("" :: Text)
    meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover" ]
    link_ [ rel_ "stylesheet", href_ "/static/login.css" ]
    link_ [ rel_ "stylesheet", href_ "/static/boxicons2.1.4.css" ]
    link_ [ rel_ "icon", type_ "image/png", href_ "/favicon-96x96.png", sizes_ "96x96"]
    link_ [ rel_ "icon", type_ "image/svg+xml", href_ "/favicon.svg"]
    link_ [ rel_ "shortcut icon", href_ "/favicon.ico"]
    link_ [ rel_ "stylesheet", href_ "/theme.css" ]
    html


login' :: Template (Html ())
login' = do
  OIDCAuthProviders providers <- asks @TemplateContext (.oidcAuthProviders)
  Phrase
    { login_button
    , login_or
    , login_password
    , login_username } <- phrase <$> asks @TemplateContext (.locale)
  themeBtn' <- themeBtn
  pure do
    div_ [ id_ "login"  ] do
      div_ [ id_ "top-bar" ] do
        themeBtn'
        localeBtn

      div_ [ id_ "login-form" ] do
        form_ [ term "hx-post" "/login"
              , term "hx-target" "#login-error"
              , term "hx-swap" "outerHTML"
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
               , term "hx-target" "this"
               , term "hx-swap" "outerHTML"
               ] mempty
          div_ $
            button_ (toHtml login_button)
        when (length providers > 0) do
          div_ [ class_ "or-sep "] (toHtml login_or)
          div_ [ class_ "panel oidc " ] do
            forM_ providers $ \provider -> do
              button_ [ onclick_  (mconcat $ ["window.location.href='", (linkToText $ apiLinks.loginAuthOIDCRedirect provider.name), "'"]) ]
                (toHtml provider.name)


localeBtn :: Html ()
localeBtn =
  div_ [ id_ "locale" ] do
    button_ [ class_ "btn btn-control "
            ] do
      span_ [ class_ "field " ] do
        i_ [ class_ "bx bx-world" ] mempty
    div_ [ class_ "dropdown-content " ] do
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just EN)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "English"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just ZH_CN)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "简体中文"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just ZH_TW)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "繁體中文"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just ZH_HK)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "繁體中文"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just JA)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "日本語"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just ES)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "Español"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just FR)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "Français"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just DE)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "Deutsch"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just KO)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "한국어"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just RU)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "Русский"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just PT)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "Português"
      div_ [ class_ "dropdown-item", term "hx-get" $ linkToText (apiLinks.loginChangeLocale (Just IT)), term "hx-target" "#login", term "hx-swap" "outerHTML" ] $ span_ "Italiano"


themeBtn :: Template (Html ())
themeBtn = do
  theme <- asks @TemplateContext (.theme)
  pure do
    case theme of
      Light -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , term "hx-get" $ linkToText apiLinks.loginToggleTheme
                , term "hx-target" "#login"
                , term "hx-swap" "outerHTML"
                ] do
          i_ [ class_ "bx bxs-moon" ] mempty
      Dark -> do
        button_ [ class_ "btn btn-control"
                , type_ "submit"
                , term "hx-get" $ linkToText apiLinks.loginToggleTheme
                , term "hx-target" "#login"
                , term "hx-swap" "outerHTML"
                ] do
          i_ [ class_ "bx bxs-sun" ] mempty


loginFailed :: Maybe Text -> Template (Html ())
loginFailed mMsg = do
  Phrase
    { login_error } <- phrase <$> asks @TemplateContext (.locale)
  pure do
    div_ [ id_ "login-error"
         , class_ "show fade-in "
         , term "hx-target" "this"
         , term "hx-swap" "outerHTML"
         ] do
           span_ (toHtml (fromMaybe login_error mMsg))
