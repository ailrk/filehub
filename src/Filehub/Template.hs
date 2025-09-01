module Filehub.Template where

import Lucid
import Data.Text (Text)
import Data.String.Interpolate (iii)


-- | The bootstrap page is used to detect the client's device  information.
--   Once we get what we need it will redirect to the real index.
bootstrap :: Html ()
bootstrap = do
  html_ do
    body_ $
      script_ [type_ "text/javascript"] $
        ([iii|
            fetch('/init', {
              method: 'POST',
              headers: {
                "Content-Type": "application/x-www-form-urlencoded",
              },
              body: new URLSearchParams({
                res: window.innerWidth + 'x' + window.innerHeight
              })
            }).then(_ => { window.location.href = '/' })
        |] :: Text)


offline :: Html ()
offline = do
  html_ do
    body_
      "offline"
