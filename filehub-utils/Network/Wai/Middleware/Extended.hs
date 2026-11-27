module Network.Wai.Middleware.Extended
  ( exposeHeaders
  , dedupHeadersKeepLast
  )
  where

import Filehub.Prelude
import Data.CaseInsensitive qualified as CI
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Header (hSetCookie)
import Network.Wai
import Network.Wai.Middleware.AddHeaders
import Prelude hiding (readFile)


exposeHeaders :: Middleware
exposeHeaders = addHeaders
  [ ("Access-Control-Expose-Headers", headers)
  ]
  where
    headers :: ByteString
    headers
      = bs_packChars
      $ mconcat
      $ intersperse ","
      [ "X-Filehub-Selected-Count"
      ]


dedupHeadersKeepLast :: Middleware
dedupHeadersKeepLast app req respond =
  app req \res ->
    respond $ mapResponseHeaders keepLastHeaders res
  where
    -- Keep the last occurrence of each header
    keepLastHeaders :: [(HeaderName, ByteString)] -> [(HeaderName, ByteString)]
    keepLastHeaders = reverse . go set_empty . reverse
      where
        go _ [] = []
        go seen ((k,v):xs)
          | k == hSetCookie = (k,v) : go seen xs
          | CI.foldedCase k `set_member` seen = go seen xs
          | otherwise = (k,v) : go (set_insert (CI.foldedCase k) seen) xs
