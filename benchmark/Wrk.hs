module Wrk where


import Control.Concurrent.Async
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


-- spawn N concurrent requests
worker :: Int -> WrkConfig -> IO [Double]
worker n cfg = mapConcurrently (\_ -> sendRequest cfg) [1..n]



-- run the benchmark with T threads, R requests per thread
runWrk :: T -> R -> WrkConfig -> IO [Double]
runWrk (T threads) (R reqsPerThread) url = concat <$> mapConcurrently (\_ -> worker reqsPerThread url) [1..threads]
