module Filehub.Cookie
  ( Cookies'(..)
  , SetCookie(..)
  , parseCookies
  , getSessionId
  , setSessionId
  , getAuthId
  , setAuthId
  , getDisplay
  , setDisplay
  , renderSetCookie
  ) where

import Web.Cookie (Cookies, SetCookie(..), defaultSetCookie)
import Web.Cookie qualified as Cookie
import Servant (FromHttpApiData (..))
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Filehub.Types (Session(..), SessionId (..), Display)
import Lens.Micro ((<&>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Filehub.Auth.Types (AuthId (..))


newtype Cookies' = Cookies' Cookies
  deriving Show


instance FromHttpApiData Cookies' where
  parseHeader = return . Cookies' . Cookie.parseCookies
  parseQueryParam = return . Cookies' . Cookie.parseCookies . Text.encodeUtf8


parseCookies :: ByteString -> Cookies'
parseCookies = Cookies' . Cookie.parseCookies


renderSetCookie :: SetCookie -> ByteString
renderSetCookie = Cookie.renderSetCookieBS


getSessionId :: Cookies' -> Maybe SessionId
getSessionId (Cookies' cookies) = go
  where
    go = lookup "sessionId" cookies >>= UUID.fromASCIIBytes <&> SessionId


setSessionId :: Session -> SetCookie
setSessionId session =
  defaultSetCookie
    { setCookieName     = "sessionId"
    , setCookieValue    = bytes
    , setCookieExpires  = Just session.expireDate
    , setCookieHttpOnly = True
    , setCookiePath     = Just "/"
    , setCookieSecure   = False -- Since there is no authentication, Secure is set to False
    }
  where
    bytes =
      let SessionId sid = session.sessionId
       in UUID.toASCIIBytes sid


getAuthId :: Cookies' -> Maybe AuthId
getAuthId (Cookies' cookies) = go
  where
    go = lookup "authId" cookies >>= UUID.fromASCIIBytes <&> AuthId



setAuthId :: Session -> Maybe SetCookie
setAuthId session = fmap mk session.authId
  where
    mk (AuthId aid) = do
      let bytes = UUID.toASCIIBytes aid
      defaultSetCookie
        { setCookieName     = "authId"
        , setCookieValue    = bytes
        , setCookieExpires  = Just session.expireDate
        , setCookieHttpOnly = True
        , setCookiePath     = Just "/"
        , setCookieSecure   = True
        }


getDisplay :: Cookies' -> Maybe Display
getDisplay (Cookies' cookies) = do
  bytes <- lookup "display" cookies
  either (const Nothing) Just $ parseUrlPiece $ Text.decodeUtf8 bytes


setDisplay :: Display -> SetCookie
setDisplay display =
  defaultSetCookie
    { setCookieName     = "display"
    , setCookieValue    = ByteString.pack $ show display
    , setCookieExpires  = Nothing
    , setCookieHttpOnly = False
    , setCookiePath     = Just "/"
    , setCookieSecure   = False
    }
