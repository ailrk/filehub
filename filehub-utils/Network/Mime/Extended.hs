module Network.Mime.Extended (isMime) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.Mime (MimeType)


isMime :: MimeType -> Text -> Bool
isMime fileMime mime = mime `T.isPrefixOf` T.decodeUtf8 fileMime
