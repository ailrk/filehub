module Filehub.UserAgent where

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString (ByteString)


data DeviceType
  = Mobile
  | Tablet
  | Bot
  | Desktop
  | Unknown
  deriving (Show, Eq)


detectDeviceType :: ByteString -> DeviceType
detectDeviceType ua
  | hasAny ["bot", "crawl", "spider", "slurp", "facebookexternalhit"] = Bot
  | hasAny ["mobile", "iphone", "android"] && not (hasAny ["ipad", "tablet"]) = Mobile
  | hasAny ["ipad", "tablet"] = Tablet
  | hasAny ["windows", "macintosh", "x11", "linux"] = Desktop
  | otherwise = Unknown
  where
    t = Text.toLower (Text.decodeUtf8 ua)
    hasAny keywords = any (`Text.isInfixOf` t) keywords
