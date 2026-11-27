module Network.Mime.Extended (isMime) where

import Filehub.Prelude
import Network.Mime (MimeType)


isMime :: MimeType -> Text -> Bool
isMime fileMime mime = mime `txt_isPrefixOf` txt_decode fileMime
