module Filehub.Cookie
  ( Cookies'(..)
  , SetCookie(..)
  , FromCookies(..)
  , parseCookies
  , renderSetCookie
  ) where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Filehub.Auth.Types (AuthId (..))
import Filehub.SharedLink (SharedLinkPermit(..))
import Filehub.Types (SessionId (..), Display)
import Servant (FromHttpApiData (..))
import Web.Cookie (Cookies, SetCookie(..))
import Web.Cookie qualified as Cookie


newtype Cookies' = Cookies' Cookies
  deriving Show


instance FromHttpApiData Cookies' where
  parseHeader     = return . Cookies' . Cookie.parseCookies
  parseQueryParam = return . Cookies' . Cookie.parseCookies . T.encodeUtf8


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
  either (const Nothing) Just (parseUrlPiece (T.decodeUtf8 bytes))


getSharedLinkPermit :: Cookies' -> Maybe SharedLinkPermit
getSharedLinkPermit (Cookies' cookies) = lookup "sharedlink_permit" cookies >>= UUID.fromASCIIBytes <&> SharedLinkPermit
