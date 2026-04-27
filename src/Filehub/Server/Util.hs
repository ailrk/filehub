module Filehub.Server.Util
  ( withQueryParam
  , parseHeader'
  )
  where


import Data.ByteString (ByteString)
import Filehub.Error (FilehubError (..))
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude hiding (readFile)
import Servant ( FromHttpApiData (..) )
import Servant.Server (err400)
import Filehub.Monad (Filehub)
import UnliftIO (throwIO)


-- | Ensure a query parameter presents, otherwise it's a client error
withQueryParam :: Maybe a -> Filehub a
withQueryParam m =
  case m of
    Just a  -> pure a
    Nothing -> throwIO do HTTPError err400


parseHeader' :: FromHttpApiData a => ByteString -> Maybe a
parseHeader' x = either (const Nothing) Just (parseHeader x)
