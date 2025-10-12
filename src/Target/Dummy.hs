{- HLINT ignore "Avoid restricted function" -}
module Target.Dummy (DummyTarget, newDummyTarget) where

import Target.Types (TargetId(..), TargetBackend, IsTarget(..))
import Data.UUID.V4 qualified as UUID
import System.IO.Unsafe (unsafePerformIO)


data DummyTarget


data instance TargetBackend DummyTarget = DummyTargetBackend TargetId


instance IsTarget DummyTarget where
  getTargetIdFromBackend (DummyTargetBackend targetId) = targetId


newDummyTarget :: TargetBackend DummyTarget
newDummyTarget = do
  let targetId = unsafePerformIO $ TargetId <$> UUID.nextRandom
  DummyTargetBackend targetId
