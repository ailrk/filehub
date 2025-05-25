module Filehub.Server.Middleware (exposeHeaders) where

import Network.Wai
import Network.Wai.Middleware.AddHeaders
import Data.List (intersperse)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString (ByteString)


exposeHeaders :: Middleware
exposeHeaders = addHeaders
  [ ("Access-Control-Expose-Headers", headers)
  ]


headers :: ByteString
headers
  = ByteString.pack
  $ mconcat
  $ intersperse ","
  [ "X-Filehub-Selected-Count"
  ]
