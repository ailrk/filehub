{-# LANGUAGE NumericUnderscores #-}
module Filehub.RateLimit
  ( RateLimiter
  , RateLimiters(..)
  , new
  , newRateLimiters
  , check
  ) where

import Filehub.Prelude
import Control.Monad (forever)
import UnliftIO.Concurrent (forkIO, threadDelay)


data RateLimiter = RateLimiter
  { maxRequests :: !Int
  , window      :: !NominalDiffTime
  , store       :: !(TVar (Map Text [UTCTime]))
  }


-- | Create a rate limiter allowing at most `maxR` requests per `win` seconds,
-- and start a background thread that evicts stale entries once per window.
new :: Int -> NominalDiffTime -> IO RateLimiter
new maxR win = do
  rl <- RateLimiter maxR win <$> newTVarIO mempty
  startEviction rl
  pure rl


-- | Returns True (allowed) or False (rate-limited).
-- Uses a sliding window: only timestamps within the last `window` seconds count.
-- The per-key list is bounded by maxRequests elements.
check :: RateLimiter -> Text -> IO Bool
check rl key = do
  now <- time_now
  atomically do
    m <- readTVar rl.store
    let ts     = fromMaybe [] (map_lookup key m)
        cutoff = time_add (negate rl.window) now
        recent = filter (> cutoff) ts
        ok     = length recent < rl.maxRequests
    if ok
      then do
        writeTVar rl.store (map_insert key (now : recent) m)
        pure True
      else pure False


-- | Remove all keys whose timestamps have all expired.  Runs in the background
-- every `window` seconds so the map does not grow without bound under a
-- distributed flood from many unique source IPs.
startEviction :: RateLimiter -> IO ()
startEviction rl = void $ forkIO $ forever do
  threadDelay intervalUs
  now <- time_now
  atomically do
    let cutoff = time_add (negate rl.window) now
    modifyStore (map_filter (any (> cutoff)))
  where
    intervalUs  = round (realToFrac rl.window * 1_000_000 :: Double)
    modifyStore f = readTVar rl.store >>= writeTVar rl.store . f


data RateLimiters = RateLimiters
  { login      :: !RateLimiter  -- brute-force: 20/min
  , sharedLink :: !RateLimiter  -- DDoS / enumeration: 300/min
  , upload     :: !RateLimiter  -- disk / bandwidth exhaustion: 30/min
  , download   :: !RateLimiter  -- bandwidth exhaustion: 120/min
  , search     :: !RateLimiter  -- CPU exhaustion (recursive FS scan): 20/min
  , global     :: !RateLimiter  -- general per-IP cap: 1000/min
  }


newRateLimiters :: IO RateLimiters
newRateLimiters = RateLimiters
  <$> new 20   60   -- login:      brute-force protection
  <*> new 300  60   -- sharedLink: enumeration / DDoS protection
  <*> new 30   60   -- upload:     disk / bandwidth exhaustion
  <*> new 3000 60   -- download:   ~60 pages of thumbnails/min (50 req/page)
  <*> new 300  60   -- search:     htmx fires on each keystroke; 5/s headroom
  <*> new 5000 60   -- global:     headroom for thumbnail-heavy browsing sessions
