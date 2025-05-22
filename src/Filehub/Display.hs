module Filehub.Display where

import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Filehub.Types ( Display (..), Resolution(..))


classify :: Resolution -> Display
classify (Resolution w h)
  | isMobile = Mobile
  | otherwise    = Desktop
  where
    aspect = fromIntegral w / fromIntegral h :: Double
    isMobile = w < 768 || aspect < 0.75
