module Filehub.Env.Internal
  ( getPort
  , getSessionPool
  , getDataDir
  , getTheme
  , getReadOnly
  , getSessionDuration
  , getTargets
  )
  where

import Effectful.Reader.Dynamic (Reader, asks)
import Effectful ((:>), Eff)
import Lens.Micro.Platform ()
import Data.Time (NominalDiffTime)
import Data.Generics.Labels ()
import Filehub.Env.Types (Env(..))
import Filehub.SessionPool.Types (SessionPool)
import Filehub.Theme (Theme)
import Filehub.Types (Target)


getPort :: (Reader Env :> es) => Eff es Int
getPort = asks @Env (.port)


getSessionPool :: (Reader Env :> es) => Eff es SessionPool
getSessionPool = asks @Env (.sessionPool)


getDataDir :: (Reader Env :> es) => Eff es FilePath
getDataDir = asks @Env (.dataDir)


getTheme :: (Reader Env :> es) => Eff es Theme
getTheme = asks @Env (.theme)


getReadOnly :: (Reader Env :> es) => Eff es Bool
getReadOnly = asks @Env (.readOnly)


getSessionDuration :: (Reader Env :> es) => Eff es NominalDiffTime
getSessionDuration = asks @Env (.sessionDuration)


getTargets :: (Reader Env :> es) => Eff es [Target]
getTargets = asks @Env (.targets)
