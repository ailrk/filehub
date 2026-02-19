{- HLINT ignore "Avoid restricted function" -}
module Target.Dummy (DummyTarget, newDummyTarget) where

import Target.Types (TargetId(..), Target, IsTarget(..))
import Data.UUID.V4 qualified as UUID
import System.IO.Unsafe (unsafePerformIO)
import Text.Debug (Debug(..))


data DummyTarget


instance Debug (Target DummyTarget) where
  debug (DummyTarget targetId) =
    mconcat [ "[<DummyTarget>, ", show targetId, "]" ]


instance IsTarget DummyTarget where
  data instance Target DummyTarget = DummyTarget TargetId
  data instance Config DummyTarget = Config ()
  getTargetIdFromBackend (DummyTarget targetId) = targetId


newDummyTarget :: Target DummyTarget
newDummyTarget = do
  let targetId = unsafePerformIO $ TargetId <$> UUID.nextRandom
  DummyTarget targetId
