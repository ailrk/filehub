{-# LANGUAGE OverloadedLabels #-}

module Filehub.Env
  ( Env(..)
  , getRoot
  , getCurrentDir
  , setCurrentDir
  , getSortFileBy
  , setSortFileBy
  , module Filehub.Env.SessionPool
  , module Filehub.Env.Internal
  , module Filehub.Env.Target
  )
  where

import Lens.Micro
import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE)
import Filehub.Types (Env(..), Session(..), SessionId)
import Filehub.Domain.Types (SortFileBy, FilehubError (..))
import Filehub.Env.SessionPool (getSession, updateSession)
import Filehub.Env.Internal (getSessionPool, getDataDir, getTheme, getSessionDuration, getTargets)
import Filehub.Env.Target (getTargetId, viewCurrentTarget, changeCurrentTarget)
import Filehub.Env.Target qualified as Target


getRoot :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Eff es FilePath
getRoot sessionId = do
  mSession <- getSession sessionId
  targets <- getTargets
  maybe (throwError InvalidSession) pure do
    index <- mSession ^? _Just . #index
    targets ^? ix index . #_FileTarget . #root


getCurrentDir :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Eff es FilePath
getCurrentDir sessionId = (^. #sessionData . #currentDir) <$> Target.viewCurrentTarget sessionId


setCurrentDir :: (Reader Env :> es, IOE :> es) => SessionId -> FilePath -> Eff es ()
setCurrentDir sessionId path = do
  updateSession sessionId $ \s -> s & #targets . ix s.index . #currentDir .~ path


getSortFileBy :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Eff es SortFileBy
getSortFileBy sessionId = (^. #sessionData . #sortedFileBy) <$> Target.viewCurrentTarget sessionId


setSortFileBy :: (Reader Env :> es, IOE :> es) => SessionId -> SortFileBy -> Eff es ()
setSortFileBy sessionId order = do
  updateSession sessionId (\s -> s & #targets . ix s.index . #sortedFileBy .~ order)
