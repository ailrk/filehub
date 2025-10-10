{- HLINT ignore "Avoid restricted function" -}
module Target.Dummy (newDummyTarget) where

import Target.Types.TargetId (TargetId(..))
import Target.Class (IsTarget (..))
import Data.UUID.V4 qualified as UUID
import System.IO.Unsafe (unsafePerformIO)


data DummyTarget


instance IsTarget DummyTarget where
  data Backend DummyTarget = DummyTargetBackend TargetId
  getTargetIdFromBackend (DummyTargetBackend targetId) = targetId


newDummyTarget :: Backend DummyTarget
newDummyTarget = do
  let targetId = unsafePerformIO $ TargetId <$> UUID.nextRandom
  DummyTargetBackend targetId
