module Wrk where


import UnliftIO.Async
import Data.Time.Clock
import Network.HTTP.Req


newtype T = T Int
newtype R = R Int


data WrkConfig =
  forall body method
  . ( HttpBody body
    , HttpMethod method
    , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
    )
    => WrkConfig
  { url     :: Url 'Http
  , method  :: method
  , headers :: Option 'Http
  , body    :: body
  }


sendRequest :: WrkConfig -> IO Double
sendRequest (WrkConfig url method headers body)= do
  start <- getCurrentTime
  _ <- runReq defaultHttpConfig $
        req method
        url
        body
        ignoreResponse
        headers
  end <- getCurrentTime
  pure $ realToFrac (diffUTCTime end start)


-- | Like sendRequest but returns (HTTP status code, latency).
-- Uses a lenient config that does not throw on non-2xx responses.
sendRequestWithStatus :: WrkConfig -> IO (Int, Double)
sendRequestWithStatus (WrkConfig url method headers body) = do
  start <- getCurrentTime
  resp  <- runReq lenientConfig $ req method url body bsResponse headers
  end   <- getCurrentTime
  pure (responseStatusCode resp, realToFrac (diffUTCTime end start))
  where
    lenientConfig = defaultHttpConfig { httpConfigCheckResponse = \_ _ _ -> Nothing }


-- spawn N concurrent requests
worker :: Int -> WrkConfig -> IO [Double]
worker n cfg = mapConcurrently (\_ -> sendRequest cfg) [1..n]


workerWithStatus :: Int -> WrkConfig -> IO [(Int, Double)]
workerWithStatus n cfg = mapConcurrently (\_ -> sendRequestWithStatus cfg) [1..n]


-- run the benchmark with T threads, R requests per thread
runWrk :: T -> R -> WrkConfig -> IO [Double]
runWrk (T threads) (R reqsPerThread) url = concat <$> mapConcurrently (\_ -> worker reqsPerThread url) [1..threads]


runWrkWithStatus :: T -> R -> WrkConfig -> IO [(Int, Double)]
runWrkWithStatus (T threads) (R reqsPerThread) cfg =
  concat <$> mapConcurrently (\_ -> workerWithStatus reqsPerThread cfg) [1..threads]
