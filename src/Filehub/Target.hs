{-# LANGUAGE DeriveGeneric #-}
module Filehub.Target
  ( TargetView(..)
  , getTargetId
  , handleTarget
  ) where

import Filehub.Types (Target(..), TargetId(..))
import Filehub.Target.Class (IsTarget(..))
import Control.Applicative (asum)
import Filehub.Target.Types.TargetView (TargetView(..))
import Filehub.Target.Types (TargetHandler, runTargetHandler)


getTargetId :: Target -> TargetId
getTargetId (Target t) = getTargetIdFromBackend t


handleTarget :: Target -> [TargetHandler r] -> Maybe r
handleTarget target handlers = asum (map (runTargetHandler target) handlers)
