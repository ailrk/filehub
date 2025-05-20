module Filehub.Template.Internal
  ( withDefault
  , bold
  , toClientPath
  , toHxVals
  )
  where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as LText
import Filehub.Types ( ClientPath(..), Display(..))
import Filehub.ClientPath qualified as ClientPath
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid


withDefault :: Display -> Html () -> Html ()
withDefault display html = do
  meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1.0" ]
  link_ [ rel_ "stylesheet", href_ "https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css" ]

  script_ [ src_ "https://unpkg.com/hyperscript.org@0.9.13" ] ("" :: Text)
  script_ [ src_ "https://unpkg.com/htmx.org@2.0.3" ] ("" :: Text)

  link_ [ rel_ "stylesheet", href_ "/static/filehub/viewer.css" ]
  script_ [ src_ "/static/filehub/viewer.js", type_ "module" ] ("" :: Text)

  case display of
    Desktop -> link_ [ rel_ "stylesheet", href_ "/static/filehub/desktop.css" ]
    Mobile -> link_ [ rel_ "stylesheet", href_ "/static/filehub/mobile.css" ]
    NoDisplay -> link_ [ rel_ "stylesheet", href_ "/static/filehub/mobile.css" ]
  script_ [ src_ "/static/filehub/ui.js", type_ "module" ] ("" :: Text)
  html


bold :: Html () -> Html ()
bold t = span_ [ class_ "bold" ] t


toClientPath :: FilePath -> FilePath -> Text
toClientPath root p = Text.pack . (.unClientPath) $ ClientPath.toClientPath root p


toHxVals :: [Pair] -> Text
toHxVals xs = (xs & Aeson.object & Aeson.encode & LText.decodeUtf8) ^. strict
