module Network.Mime.Extended (isMime) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Network.Mime (MimeType)


isMime :: MimeType -> Text -> Bool
isMime fileMime mime = mime `T.isPrefixOf` decodeUtf8 fileMime
