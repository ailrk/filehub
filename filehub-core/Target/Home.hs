{-# LANGUAGE TypeFamilies #-}
module Target.Home (Home, newHomeTarget) where

import Filehub.Prelude
import Target.Types (TargetId(..), Target, IsTarget(..), HasTargetId(..))
import Data.UUID.V4 qualified as UUID
import Text.Debug (Debug(..))


data Home


instance Debug (Target Home) where
  debug (HomeTarget tid) = mconcat [ "[<HomeTarget>, ", show tid, "]" ]


instance HasTargetId (Target Home) where
  getTargetId (HomeTarget tid) = tid


instance IsTarget Home where
  data instance Target Home = HomeTarget TargetId
  data instance Config Home = Config ()


newHomeTarget :: IO (Target Home)
newHomeTarget = HomeTarget . TargetId <$> UUID.nextRandom
