{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Target.Types
  ( TargetId(..)
  , Target(..)
  )
  where

import Filehub.Target.Types.TargetId (TargetId(..))
import Filehub.Target.Class (TargetBackend(..))


data Target where
  MkTarget :: TargetBackend b => b -> Target


instance Eq Target where
  MkTarget a == MkTarget b = getTargetId a == getTargetId b


instance Show Target where
  show (MkTarget a) = "Target (" ++ show (getTargetId a) ++ ")"
