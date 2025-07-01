{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Filehub.Target.Types where
import Filehub.Target.Class (IsTarget(..))
import Data.Typeable (Typeable, cast)


data Target where
  Target :: (Typeable a, IsTarget a) => Backend a -> Target


data TargetHandler r = forall a. (Typeable a, IsTarget a) => TargetHandler (Backend a -> r)


targetHandler :: forall a r. (Typeable a, IsTarget a) => (Backend a -> r) -> TargetHandler r
targetHandler = TargetHandler


runTargetHandler :: Target -> TargetHandler r -> Maybe r
runTargetHandler (Target t) (TargetHandler f) = fmap f (cast t)
