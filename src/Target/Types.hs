{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Target.Types
  ( Target(..)
  , TargetHandler(..)
  , TargetId(..)
  , targetHandler
  , runTargetHandler
  , getTargetId
  , handleTarget
  )
  where

import Data.Generics.Labels ()
import Data.Typeable (Typeable, cast)
import Lens.Micro.Platform ()
import Prelude hiding (readFile, writeFile)
import Target.Class (IsTarget(..))
import Target.Types.TargetId (TargetId(..))
import Control.Applicative (asum)


-- | Existential wrapper of `Backend a`.
data Target where
  Target :: (Typeable a, IsTarget a) => Backend a -> Target


data TargetHandler r = forall a. (Typeable a, IsTarget a) => TargetHandler (Backend a -> r)


targetHandler :: forall a r. (Typeable a, IsTarget a) => (Backend a -> r) -> TargetHandler r
targetHandler = TargetHandler


runTargetHandler :: Target -> TargetHandler r -> Maybe r
runTargetHandler (Target t) (TargetHandler f) = fmap f (cast t)



getTargetId :: Target -> TargetId
getTargetId (Target t) = getTargetIdFromBackend t


handleTarget :: Target -> [TargetHandler r] -> Maybe r
handleTarget target handlers = asum (map (runTargetHandler target) handlers)
