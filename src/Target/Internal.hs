{-# LANGUAGE DeriveGeneric #-}
module Target.Internal
  ( getTargetId
  , handleTarget
  ) where

import Target.Class (IsTarget(..))
import Control.Applicative (asum)
import Target.Types (TargetHandler, runTargetHandler, Target(..))
import Target.Types.TargetId (TargetId)


getTargetId :: Target -> TargetId
getTargetId (Target t) = getTargetIdFromBackend t


handleTarget :: Target -> [TargetHandler r] -> Maybe r
handleTarget target handlers = asum (map (runTargetHandler target) handlers)
