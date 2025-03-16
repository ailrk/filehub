module Filehub.Env
  ( Env(..)
  , getRoot
  , getPort
  , getCurrentDir
  , setCurrentDir
  , setSortFileBy
  , getSortFileBy
  , getDataDir
  , getTheme
  )
  where
import {-# SOURCE #-} Filehub.Domain (File, Theme, SortFileBy)
import Effectful.Reader.Dynamic (Reader, asks)
import Effectful ((:>), Eff)
import Effectful.Concurrent.STM


data Env = Env
  { root :: !FilePath
  , port :: !Int
  , rootTree :: TVar File
  , currentDir :: TVar FilePath
  , sortFileBy :: TVar SortFileBy
  , dataDir :: !FilePath
  , theme :: Theme
  }


getRoot :: (Reader Env :> es) => Eff es FilePath
getRoot = asks @Env (.root)


getPort :: (Reader Env :> es) => Eff es Int
getPort = asks @Env (.port)


getCurrentDir :: (Reader Env :> es, Concurrent :> es) => Eff es FilePath
getCurrentDir = asks @Env (.currentDir) >>= readTVarIO


setCurrentDir :: (Reader Env :> es, Concurrent :> es) => FilePath -> Eff es ()
setCurrentDir path = do
  ref <- asks @Env (.currentDir)
  atomically $ ref `modifyTVar` const path


setSortFileBy :: (Reader Env :> es, Concurrent :> es) => SortFileBy -> Eff es ()
setSortFileBy order = do
  ref <- asks @Env (.sortFileBy)
  atomically $ ref `modifyTVar` const order


getSortFileBy :: (Reader Env :> es, Concurrent :> es) => Eff es SortFileBy
getSortFileBy = do
  ref <- asks @Env (.sortFileBy)
  readTVarIO ref


getDataDir :: (Reader Env :> es) => Eff es FilePath
getDataDir = asks @Env (.dataDir)


getTheme :: (Reader Env :> es) => Eff es Theme
getTheme = asks @Env (.theme)
