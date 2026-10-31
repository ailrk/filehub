module Network.HTTP.Headers.Extended where

import Web.HttpApiData (FromHttpApiData (..))
import Data.ByteString (ByteString)


parseHeader' :: FromHttpApiData a => ByteString -> Maybe a
parseHeader' x = either (const Nothing) Just (parseHeader x)
