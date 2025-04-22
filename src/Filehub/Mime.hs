module Filehub.Mime (isMime) where


import Data.Text (Text)
import Data.Text qualified as Text
import Network.Mime (MimeType)
import Data.Text.Encoding qualified as Text


isMime :: MimeType -> Text -> Bool
isMime fileMime mime = mime `Text.isPrefixOf` Text.decodeUtf8 fileMime
