module Filehub.Env
  ( Env(..)
  , getRoot
  , getCurrentDir
  , setCurrentDir
  , getSortFileBy
  , setSortFileBy
  , getDisplay
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
    ( Env(..), Session(..), SessionId, Target(..), SortFileBy, Display (..))
import Filehub.Error (FilehubError (..))
import Filehub.SessionPool (getSession, updateSession)
import Filehub.Env.Internal (getSessionPool, getDataDir, getTheme, getReadOnly, getSessionDuration, getTargets)
import Filehub.Target (currentTarget, changeCurrentTarget, TargetView (..))
import Filehub.Target qualified as Target
import Filehub.Display qualified as Display
import Filehub.Target.File (Backend(..), FileSys)
import Filehub.Target.S3 (S3)
import Control.Applicative (asum)
import Data.Typeable (cast)
import Data.Maybe (fromMaybe)


getRoot :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es FilePath
getRoot sessionId = do
  TargetView (Target t) _ _ <- currentTarget sessionId
  pure $
    fromMaybe "" . asum $
      [ cast t <&> \(x :: Backend FileSys) -> x.root
      , cast t <&> \(_ :: Backend S3) -> ""
      ]


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


getDisplay :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es Display
getDisplay sessionId = do
  session <- getSession sessionId
  case session ^. #resolution of
    Just resolution -> pure $ Display.classify resolution
    Nothing -> pure NoDisplay
