module Filehub.Template where


import Lucid
import Data.Text (Text)
import Data.String.Interpolate (iii)


-- | The bootstrap page is used to detect the client's device  information.
--   Once we get what we need it will redirect to the real index.
bootstrap :: Html ()
bootstrap = do
  html_ $ do
    body_ $
      script_ [type_ "text/javascript"] $
        ([iii|
            fetch('/init?res=' + window.innerWidth + 'x' + window.innerHeight)
              .then(_ => {
                window.location.href = '/';
              })
        |] :: Text)
