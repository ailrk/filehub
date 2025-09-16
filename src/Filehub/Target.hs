{-# LANGUAGE DeriveGeneric #-}
module Filehub.Target
  ( TargetView(..)
  , getTargetId
  , handleTarget
  ) where

import Target.Class (IsTarget(..))
import Control.Applicative (asum)
import Target.Types.TargetView (TargetView(..))
import Target.Types (TargetHandler, runTargetHandler, Target (..), TargetId)


getTargetId :: Target -> TargetId
getTargetId (Target t) = getTargetIdFromBackend t


handleTarget :: Target -> [TargetHandler r] -> Maybe r
handleTarget target handlers = asum (map (runTargetHandler target) handlers)
