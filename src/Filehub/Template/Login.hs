module Filehub.Template.Login where


import Lucid
import Data.Text (Text)
import Filehub.Auth.Simple (SimpleAuthUserDB(..))
import Filehub.Auth.OIDC (OIDCAuthProviders(..))
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Control.Monad (when)
import Data.Foldable (forM_)


login :: SimpleAuthUserDB
      -> OIDCAuthProviders
      ->  Html ()
login _ (OIDCAuthProviders providers) = do
  meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0, viewport-fit=cover" ]
  link_ [ rel_ "stylesheet", href_ "/static/login.css" ]
  link_ [ rel_ "icon", type_ "image/png", href_ "/favicon-96x96.png", sizes_ "96x96"]
  link_ [ rel_ "icon", type_ "image/svg+xml", href_ "/favicon.svg"]
  link_ [ rel_ "shortcut icon", href_ "/favicon.ico"]
  link_ [ rel_ "stylesheet", href_ "/theme.css" ]
  script_ [ src_ "/static/htmx2.0.3.js" ] ("" :: Text)

  div_ [ id_ "login"  ] do
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
               , placeholder_ "Username"
               , autocomplete_ "new-password"
               ]
      div_ do
        input_ [ type_ "password"
               , id_ "password"
               , name_ "password"
               , placeholder_ "Password"
               , autocomplete_ "off"
               ]
      div_ [ id_ "login-error"
           , term "hx-target" "this"
           , term "hx-swap" "outerHTML"
           ] mempty
      div_ $
        button_ "Login"

    when (length providers > 0) do
      div_ [ class_ "or-sep "] "or"
      div_ [ class_ "panel oidc " ] do
        forM_ providers $ \provider -> do
          button_ (toHtml provider.name)


loginFailed :: Html ()
loginFailed =
      div_ [ id_ "login-error"
           , class_ "show fade-in "
           , term "hx-target" "this"
           , term "hx-swap" "outerHTML"
           ] do
    span_ "Incorrect username or password"
