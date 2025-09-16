{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Target.Types
  ( Target(..)
  , TargetHandler(..)
  , targetHandler
  , runTargetHandler
  )
  where

import Target.Class (IsTarget(..))
import Data.Typeable (Typeable, cast)
import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Prelude hiding (readFile, writeFile)


-- | Existential wrapper of `Backend a`.
data Target where
  Target :: (Typeable a, IsTarget a) => Backend a -> Target


data TargetHandler r = forall a. (Typeable a, IsTarget a) => TargetHandler (Backend a -> r)


targetHandler :: forall a r. (Typeable a, IsTarget a) => (Backend a -> r) -> TargetHandler r
targetHandler = TargetHandler


runTargetHandler :: Target -> TargetHandler r -> Maybe r
runTargetHandler (Target t) (TargetHandler f) = fmap f (cast t)
