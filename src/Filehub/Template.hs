module Filehub.Template where

import Lucid
import Data.Text (Text)


withDefault :: Html () -> Html ()
withDefault html = do
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
  link_ [rel_ "stylesheet", href_ "/static/style.css" ]
  link_ [rel_ "stylesheet", href_ "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css" ]

  script_ [src_ "https://unpkg.com/hyperscript.org@0.9.13"] ("" :: Text)
  script_ [src_ "https://unpkg.com/htmx.org@2.0.3"] ("" :: Text)
  html
