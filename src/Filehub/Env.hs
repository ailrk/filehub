module Filehub.Env
  ( Env(..)
  , getRoot
  , getCurrentDir
  , setCurrentDir
  , getSortFileBy
  , setSortFileBy
  , module Filehub.SessionPool
  , module Filehub.Env.Internal
  , module Filehub.Target
  )
  where

import Lens.Micro
import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE)
import Effectful.Log (Log)
import Filehub.Types
    ( Env(..), Session(..), SessionId, Target(..), SortFileBy)
import Filehub.Error (FilehubError (..))
import Filehub.SessionPool (getSession, updateSession)
import Filehub.Env.Internal (getSessionPool, getDataDir, getTheme, getReadOnly, getSessionDuration, getTargets)
import Filehub.Target (getTargetId, currentTarget, changeCurrentTarget, TargetView (..))
import Filehub.Target qualified as Target


getRoot :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es FilePath
getRoot sessionId = do
  TargetView target _ _ <- currentTarget sessionId
  case target of
    FileTarget _ -> pure $ target ^. #_FileTarget . #root
    S3Target  _ -> pure ""


getCurrentDir :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es FilePath
getCurrentDir sessionId = (^. #sessionData . #currentDir) <$> Target.currentTarget sessionId


setCurrentDir :: (Reader Env :> es, IOE :> es) => SessionId -> FilePath -> Eff es ()
setCurrentDir sessionId path = do
  updateSession sessionId $ \s -> s & #targets . ix s.index . #currentDir .~ path


getSortFileBy :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es SortFileBy
getSortFileBy sessionId = (^. #sessionData . #sortedFileBy) <$> Target.currentTarget sessionId


setSortFileBy :: (Reader Env :> es, IOE :> es) => SessionId -> SortFileBy -> Eff es ()
setSortFileBy sessionId order = do
  updateSession sessionId (\s -> s & #targets . ix s.index . #sortedFileBy .~ order)
