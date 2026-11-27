module Network.HTTP.Headers.Extended where

import Filehub.Prelude
import Web.HttpApiData (FromHttpApiData (..))


parseHeader' :: FromHttpApiData a => ByteString -> Maybe a
parseHeader' x = either (const Nothing) Just (parseHeader x)
