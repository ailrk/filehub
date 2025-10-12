module Filehub.Cookie
  ( Cookies'(..)
  , SetCookie(..)
  , FromCookies(..)
  , parseCookies
  , renderSetCookie
  ) where

import Web.Cookie (Cookies, SetCookie(..))
import Web.Cookie qualified as Cookie
import Servant (FromHttpApiData (..))
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Filehub.Types (SessionId (..), Display)
import Lens.Micro ((<&>))
import Data.ByteString (ByteString)
import Filehub.Auth.Types (AuthId (..))
import Filehub.SharedLink (SharedLinkPermit(..))


newtype Cookies' = Cookies' Cookies
  deriving Show


instance FromHttpApiData Cookies' where
  parseHeader     = return . Cookies' . Cookie.parseCookies
  parseQueryParam = return . Cookies' . Cookie.parseCookies . Text.encodeUtf8


class FromCookies a where
  fromCookies :: Cookies' -> Maybe a


parseCookies :: ByteString -> Cookies'
parseCookies = Cookies' . Cookie.parseCookies


renderSetCookie :: SetCookie -> ByteString
renderSetCookie = Cookie.renderSetCookieBS


instance FromCookies SessionId where
  fromCookies = getSessionId


instance FromCookies AuthId where
  fromCookies = getAuthId


instance FromCookies Display where
  fromCookies = getDisplay


instance FromCookies SharedLinkPermit where
  fromCookies = getSharedLinkPermit


getSessionId :: Cookies' -> Maybe SessionId
getSessionId (Cookies' cookies) = lookup "sessionId" cookies >>= UUID.fromASCIIBytes <&> SessionId


getAuthId :: Cookies' -> Maybe AuthId
getAuthId (Cookies' cookies) = lookup "authId" cookies >>= UUID.fromASCIIBytes <&> AuthId


getDisplay :: Cookies' -> Maybe Display
getDisplay (Cookies' cookies) = do
  bytes <- lookup "display" cookies
  either (const Nothing) Just (parseUrlPiece (Text.decodeUtf8 bytes))


getSharedLinkPermit :: Cookies' -> Maybe SharedLinkPermit
getSharedLinkPermit (Cookies' cookies) = lookup "sharedlink_permit" cookies >>= UUID.fromASCIIBytes <&> SharedLinkPermit
