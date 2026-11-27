module Filehub.UserAgent where

import Filehub.Prelude


data DeviceType
  = Mobile
  | Tablet
  | Bot
  | Desktop
  | Unknown
  deriving (Show, Eq)


detectDeviceType :: ByteString -> DeviceType
detectDeviceType ua
  | hasAny ["bot", "crawl", "spider", "slurp", "facebookexternalhit"]         = Bot
  | hasAny ["mobile", "iphone", "android"] && not (hasAny ["ipad", "tablet"]) = Mobile
  | hasAny ["ipad", "tablet"]                                                 = Tablet
  | hasAny ["windows", "macintosh", "x11", "linux"]                           = Desktop
  | otherwise                                                                 = Unknown
  where
    t = txt_toLower (txt_decode ua)
    hasAny keywords = any (`txt_isInfixOf` t) keywords
