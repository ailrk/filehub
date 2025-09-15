module Network.Wai.Middleware.Extended where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.CaseInsensitive qualified as CI
import Data.List (intersperse)
import Data.Set qualified as Set
import Network.HTTP.Types (HeaderName)
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
      = ByteString.pack
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
    keepLastHeaders = reverse . go Set.empty . reverse
      where
        go _ [] = []
        go seen ((k,v):xs)
          | CI.foldedCase k `Set.member` seen = go seen xs
          | otherwise = (k,v) : go (Set.insert (CI.foldedCase k) seen) xs
