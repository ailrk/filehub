{- HLINT ignore "Avoid restricted function" -}
{-# LANGUAGE TypeFamilies #-}
module Target.Dummy (DummyTarget, newDummyTarget) where

import Filehub.Prelude
import Target.Types (TargetId(..), Target, IsTarget(..), HasTargetId(..))
import Data.UUID.V4 qualified as UUID
import System.IO.Unsafe (unsafePerformIO)
import Text.Debug (Debug(..))


data DummyTarget


instance Debug (Target DummyTarget) where
  debug (DummyTarget targetId) =
    mconcat [ "[<DummyTarget>, ", show targetId, "]" ]


instance HasTargetId (Target DummyTarget) where
  getTargetId (DummyTarget targetId) = targetId


instance IsTarget DummyTarget where
  data instance Target DummyTarget = DummyTarget TargetId
  data instance Config DummyTarget = Config ()


newDummyTarget :: Target DummyTarget
newDummyTarget = do
  let targetId = unsafePerformIO $ TargetId <$> UUID.nextRandom
  DummyTarget targetId
