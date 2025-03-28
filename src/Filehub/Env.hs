module Filehub.Env
  ( Env(..)
  , getRoot
  , getPort
  , getCurrentDir
  , setCurrentDir
  , setSortFileBy
  , getSortFileBy
  , getSessionPool
  , getDataDir
  , getTheme
  , getSessionDuration
  )
  where

import Effectful.Reader.Dynamic (Reader, asks)
import Effectful ((:>), Eff, IOE)
import Filehub.Types (Env(..), SessionPool(..), Session(..), SessionId)
import Filehub.Domain.Types (SortFileBy, Theme)
import Filehub.SessionPool qualified as SessionPool
import Data.Time (NominalDiffTime)


getRoot :: (Reader Env :> es) => Eff es FilePath
getRoot = asks @Env (.root)


getPort :: (Reader Env :> es) => Eff es Int
getPort = asks @Env (.port)


getCurrentDir :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es (Maybe FilePath)
getCurrentDir sessionId = (fmap (.currentDir)) <$> SessionPool.getSession sessionId


setCurrentDir :: (Reader Env :> es, IOE :> es) => SessionId -> FilePath -> Eff es ()
setCurrentDir sessionId path =
  SessionPool.updateSession sessionId (\s -> s { currentDir = path})


setSortFileBy :: (Reader Env :> es, IOE :> es) => SessionId -> SortFileBy -> Eff es ()
setSortFileBy sessionId order = do
  SessionPool.updateSession sessionId (\s -> s { sortedFileBy = order })


getSortFileBy :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es (Maybe SortFileBy)
getSortFileBy sessionId = (fmap (.sortedFileBy)) <$> SessionPool.getSession sessionId


getSessionPool :: (Reader Env :> es) => Eff es SessionPool
getSessionPool = asks @Env (.sessionPool)


getDataDir :: (Reader Env :> es) => Eff es FilePath
getDataDir = asks @Env (.dataDir)


getTheme :: (Reader Env :> es) => Eff es Theme
getTheme = asks @Env (.theme)


getSessionDuration :: (Reader Env :> es) => Eff es NominalDiffTime
getSessionDuration = asks @Env (.sessionDuration)
