module Filehub.Cookie (Cookies'(..)) where

import Web.Cookie (Cookies, parseCookies)
import Servant (FromHttpApiData (..))
import Data.Text.Encoding qualified as T


newtype Cookies' = Cookies' Cookies


instance FromHttpApiData Cookies' where
  parseHeader = return . Cookies' . parseCookies
  parseQueryParam = return . Cookies' . parseCookies . T.encodeUtf8
