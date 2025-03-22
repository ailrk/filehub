module Filehub.Env where

import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE)
import Filehub.Types (Env(..), SessionPool(..), SessionId)
import Filehub.Domain.Types (SortFileBy, Theme)


getRoot :: (Reader Env :> es) => Eff es FilePath
getPort :: (Reader Env :> es) => Eff es Int
getCurrentDir :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es FilePath
setCurrentDir :: (Reader Env :> es, IOE :> es) => SessionId -> FilePath -> Eff es ()
setSortFileBy :: (Reader Env :> es, IOE :> es) => SessionId -> SortFileBy -> Eff es ()
getSortFileBy :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es SortFileBy
getSessionPool :: (Reader Env :> es) => Eff es SessionPool
getDataDir :: (Reader Env :> es) => Eff es FilePath
getTheme :: (Reader Env :> es) => Eff es Theme
