module Filehub.Cookie
  ( Cookies'(..)
  , SetCookie(..)
  , parseCookies
  , getSessionId
  , setSessionId
  , getResolution
  , renderSetCookie
  ) where

import Web.Cookie (Cookies, SetCookie(..), defaultSetCookie)
import Web.Cookie qualified as Cookie
import Servant (FromHttpApiData (..))
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Filehub.Types (Session(..), SessionId (..), Resolution)
import Lens.Micro ((<&>))
import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as Text


newtype Cookies' = Cookies' Cookies
  deriving Show


instance FromHttpApiData Cookies' where
  parseHeader = return . Cookies' . Cookie.parseCookies
  parseQueryParam = return . Cookies' . Cookie.parseCookies . T.encodeUtf8


parseCookies :: ByteString -> Cookies'
parseCookies = Cookies' . Cookie.parseCookies


renderSetCookie :: SetCookie -> ByteString
renderSetCookie = Cookie.renderSetCookieBS


getSessionId :: Cookies' -> Maybe SessionId
getSessionId (Cookies' cookies) = go
  where
    go = lookup "sessionId" cookies >>= UUID.fromASCIIBytes <&> SessionId


getResolution :: Cookies' -> Maybe Resolution
getResolution (Cookies' cookies) = go
  where
    go = do
      raw <- lookup "resolution" cookies <&> Text.decodeUtf8
      either (const Nothing) Just $ parseUrlPiece raw


setSessionId :: Session -> SetCookie
setSessionId session =
  defaultSetCookie
    { setCookieName = "sessionId"
    , setCookieValue = bytes
    , setCookieExpires = Just session.expireDate
    , setCookieHttpOnly = True
    , setCookieSecure = False -- Since there is no authentication, Secure is set to False
    }
  where
    bytes =
      let SessionId sid = session.sessionId
       in UUID.toASCIIBytes sid
