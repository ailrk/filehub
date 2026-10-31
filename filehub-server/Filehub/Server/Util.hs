{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Filehub.Server.Util
  ( withQueryParam
  , throttle
  )
  where

import Filehub.Error (FilehubError (..))
import Prelude hiding (elem)
import Prelude hiding (readFile)
import Servant.Server (err400)
import Filehub.Monad (Filehub)
import UnliftIO (throwIO)
import Control.Monad (when)


-- | Ensure a query parameter presents, otherwise it's a client error
withQueryParam :: Maybe a -> Filehub a
withQueryParam m =
  case m of
    Just a  -> pure a
    Nothing -> throwIO do HTTPError err400


throttle :: (Monad m, Integral n) => n -> n -> m () -> m ()
throttle idx total action
  | total <= 0 = action
  | otherwise  =
      let
          steps = if
                     | total > 1000 -> total `div` 50
                     | total > 100  -> 20
                     | total > 10   -> 8
                     | otherwise    -> 3

          -- Ensure interval is at least 1 to avoid DivByZero
          interval = max 1 (total `div` steps)
      in
          when (idx `rem` interval == 0 || idx == total) action
