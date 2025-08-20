{-# LANGUAGE DeriveGeneric #-}
module Filehub.Target
  ( TargetView(..)
  , getTargetId
  , handleTarget
  , currentTarget
  , changeCurrentTarget
  , withTarget
  ) where

import Filehub.Types
    ( Target(..),
      TargetId(..),
      Env(..),
      SessionId,
      Target)
import Data.List (find)
import Data.Generics.Labels ()
import Data.String.Interpolate (i)
import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Log (Log, logAttention, logTrace_)
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.SessionPool qualified as SessionPool
import Filehub.Env.Internal qualified as Env
import Filehub.Target.Class (IsTarget(..))
import Control.Applicative (asum)
import Filehub.Target.Types.TargetView (TargetView(..))
import Filehub.Target.Types (TargetHandler, runTargetHandler)


getTargetId :: Target -> TargetId
getTargetId (Target t) = getTargetIdFromBackend t


handleTarget :: Target -> [TargetHandler r] -> Maybe r
handleTarget target handlers = asum (map (runTargetHandler target) handlers)


currentTarget :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es TargetView
currentTarget sessionId = do
  mSession <- SessionPool.getSession sessionId
  targets <- Env.getTargets
  maybe (throwError (FilehubError InvalidSession "Invalid session")) pure do
    index <- mSession ^? #index
    targetSessionData <- mSession ^? #targets . ix index
    target <- targets ^? ix index
    pure $ TargetView target targetSessionData index


changeCurrentTarget :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> TargetId -> Eff es ()
changeCurrentTarget sessionId targetId = do
  logTrace_ [i|Changing target to #{targetId}|]
  TargetView target _ _ <- currentTarget sessionId
  targets <- Env.getTargets
  if getTargetId target == targetId
     then pure ()
     else do
       case find (\(_, x) -> getTargetId x == targetId) ([0..] `zip` targets) of
         Just (idx, _) -> do
           SessionPool.updateSession sessionId (\s -> s & #index .~ idx)
         Nothing -> do
           logAttention "Can't find target" (show targetId)
           throwError (FilehubError InvalidSession "Invalid session")


withTarget :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> TargetId -> (TargetView -> Eff es a) -> Eff es a
withTarget sessionId targetId action = do
  TargetView saved _ _ <- currentTarget sessionId
  changeCurrentTarget sessionId targetId
  result <- currentTarget sessionId >>= action
  changeCurrentTarget sessionId (getTargetId saved)
  pure result
