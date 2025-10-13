module Target.Virtual where

import Target.Types (TargetId(..), TargetBackend)
import Data.Text (Text)
import Effectful (IOE, (:>), Eff)
import Text.Debug (Debug(..))


-- TODO
-- - vtarget need to be backed by other targets
-- - each node is either a virtual node, or a node backed by another target.
-- - if it's a vnode, you can't add files
-- - if it's a backed node, you can do everything you want
-- - vtarget can be backed by other vtarget, creating a chain
-- - vtarget's corresponding implementation is vstorage
-- - vstorage works on the vstore, which is a virtual store


data Virtual


data instance TargetBackend Virtual =
  VirtualBackend
    { targetId   :: TargetId
    , targetName :: Maybe Text
    }


instance Debug (TargetBackend Virtual) where
  debug VirtualBackend {} =
    mconcat
      [ "<VirtualBackend: "
      , ">"
      ]


data Config = Config
  { root :: FilePath
  }
  deriving (Show, Eq)


instance Debug Config where debug = show


initialize :: (IOE :> es) => Config -> Eff es (TargetBackend Virtual)
initialize opt = error "not implemented"
