{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Data.ClientPath.View where

import Data.ClientPath (ClientPath, Root, AbsPath, toClientPath, fromClientPath)
import Data.Hashable (Hashable(..))


data ClientPathView = ClientPathView
  { clientPath :: ClientPath
  , hashPath   :: Word
  , path       :: AbsPath
  }


class AsClientPathView a where
  asClientPathView :: Root -> a -> ClientPathView


instance AsClientPathView AbsPath where
  asClientPathView root path =
    let clientPath = toClientPath root path
     in ClientPathView
          { clientPath = clientPath
          , hashPath   = fromIntegral @_ @Word (hash clientPath)
          , path       = path
          }
  {-# INLINE asClientPathView #-}


instance AsClientPathView ClientPath where
  asClientPathView root cp =
    let path = fromClientPath root cp
     in ClientPathView
          { clientPath = cp
          , hashPath   = fromIntegral @_ @Word (hash cp)
          , path       = path
          }
  {-# INLINE asClientPathView #-}
