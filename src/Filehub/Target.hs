module Filehub.Target where

import Effectful (IOE, (:>), Eff)
import Effectful.Log (Log)
import Target.Types (Target)


createTarget :: (IOE :> es, Log :> es) => Eff es Target
createTarget = undefined


