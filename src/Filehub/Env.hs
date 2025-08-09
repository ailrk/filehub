{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Filehub.Env
  ( Env(..)
  , getRoot
  , getCurrentDir
  , setCurrentDir
  , getSortFileBy
  , setSortFileBy
  , getLayout
  , setLayout
  , getSessionTheme
  , setSessionTheme
  , getDisplay
  , module Filehub.SessionPool
  , module Filehub.Env.Internal
  , module Filehub.Target
  )
  where

import Lens.Micro
import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Filehub.Types
    ( Env(..), Session(..), Target(..), Display (..))
import Filehub.SessionPool (getSession, updateSession)
import Filehub.Env.Internal (getSessionPool, getTheme, getReadOnly, getSessionDuration, getTargets)
import Filehub.Target (currentTarget, changeCurrentTarget)
import Filehub.Target qualified as Target
import Filehub.Display qualified as Display
import Filehub.Target.File (Backend(..), FileSys)
import Filehub.Target.S3 (S3)
import Filehub.UserAgent qualified as UserAgent
import Filehub.Target.Types.TargetView (TargetView(..))
import Control.Applicative (asum)
import Data.Typeable (cast)
import Data.Maybe (fromMaybe)


getRoot sessionId = do
  TargetView (Target t) _ _ <- currentTarget sessionId
  pure $
    fromMaybe "" . asum $
      [ cast t <&> \(x :: Backend FileSys) -> x.root
      , cast t <&> \(_ :: Backend S3) -> ""
      ]


getCurrentDir sessionId = (^. #sessionData . #currentDir) <$> Target.currentTarget sessionId


setCurrentDir sessionId path = do
  updateSession sessionId $ \s -> s & #targets . ix s.index . #currentDir .~ path


getSortFileBy sessionId = (^. #sessionData . #sortedFileBy) <$> Target.currentTarget sessionId


setSortFileBy sessionId order = do
  updateSession sessionId (\s -> s & #targets . ix s.index . #sortedFileBy .~ order)


getLayout sessionId = (^. #layout) <$> getSession sessionId


setLayout sessionId layout = do
  updateSession sessionId (\s -> s & #layout .~ layout)


getSessionTheme sessionId = (^. #theme) <$> getSession sessionId


setSessionTheme sessionId theme = do
  updateSession sessionId (\s -> s & #theme .~ theme)



getDisplay sessionId = do
  session <- getSession sessionId
  case session ^. #resolution of
    Just resolution ->
      case session ^. #deviceType of
        UserAgent.Desktop -> pure Desktop
        _ -> pure $ Display.classify resolution
    Nothing -> pure NoDisplay
