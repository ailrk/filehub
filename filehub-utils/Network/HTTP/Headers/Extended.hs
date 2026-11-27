module Network.HTTP.Headers.Extended where

import Data.ByteString (ByteString)
import Web.HttpApiData (FromHttpApiData (..))


parseHeader' :: FromHttpApiData a => ByteString -> Maybe a
parseHeader' x = either (const Nothing) Just (parseHeader x)
