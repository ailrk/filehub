{-# LANGUAGE TypeFamilies #-}
module Target.Home (Home, newHomeTarget) where

import Filehub.Prelude
import Target.Types (TargetId(..), Target, IsTarget(..), HasTargetId(..))
import Data.UUID.V5 qualified as UUID
import Text.Debug (Debug(..))


data Home


instance Debug (Target Home) where
  debug (HomeTarget tid) = mconcat [ "[<HomeTarget>, ", show tid, "]" ]


instance HasTargetId (Target Home) where
  getTargetId (HomeTarget tid) = tid


instance IsTarget Home where
  data instance Target Home = HomeTarget TargetId
  data instance Config Home = Config ()


homeTargetId :: TargetId
homeTargetId = TargetId $ UUID.generateNamed UUID.namespaceDNS (bs_unpack $ txt_encode (txt_pack "filehub.target.home"))

newHomeTarget :: Target Home
newHomeTarget = HomeTarget homeTargetId
