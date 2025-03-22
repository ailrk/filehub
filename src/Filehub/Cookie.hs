module Filehub.Cookie (Cookies'(..), SetCookie(..), getSessionId, setSessionId) where

import Web.Cookie (Cookies, SetCookie(..), parseCookies, defaultSetCookie)
import Servant (FromHttpApiData (..))
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Filehub.Types (Session(..), SessionId (..))
import Lens.Micro ((<&>))


newtype Cookies' = Cookies' Cookies


instance FromHttpApiData Cookies' where
  parseHeader = return . Cookies' . parseCookies
  parseQueryParam = return . Cookies' . parseCookies . T.encodeUtf8


getSessionId :: Cookies' -> Maybe SessionId
getSessionId (Cookies' cookies) =
  lookup "sessionId" cookies >>= UUID.fromASCIIBytes <&> SessionId


setSessionId :: Session -> SetCookie
setSessionId session =
  defaultSetCookie
    { setCookieName = "sessionId"
    , setCookieValue = bytes
    , setCookieExpires = Just session.expireDate
    -- , setCookieHttpOnly = True
    -- , setCookieSecure = True
    }
  where
    bytes =
      let SessionId sid = session.sessionId
       in UUID.toASCIIBytes sid
