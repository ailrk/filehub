module Filehub.Env
  ( Env(..)
  , hasNoLogin
  )
  where

import Data.Map.Strict qualified as Map
import Lens.Micro.Platform ()
import Filehub.Types ( Env(..))
import Filehub.Auth.Simple (SimpleAuthUserDB(..))


-- | Check if there is login information provided when the program starts.
hasNoLogin :: Env -> Bool
hasNoLogin (Env { simpleAuthUserDB = SimpleAuthUserDB db }) = Map.null db
