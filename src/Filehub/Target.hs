{-# LANGUAGE DeriveGeneric #-}
module Filehub.Target
  ( TargetView(..)
  , getTargetId
  , currentTarget
  , changeCurrentTarget
  , withTarget
  , getS3Target
  ) where

import Filehub.Types
    ( Target(..),
      TargetSessionData(..),
      FileTarget(..),
      S3Target(..),
      TargetId(..),
      Env(..),
      SessionId,
      Target,
      TargetSessionData )
import Data.List (find)
import Data.Generics.Labels ()
import Data.String.Interpolate (i)
import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Log (Log, logAttention, logTrace_)
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import GHC.Generics (Generic)
import Filehub.Error (FilehubError (..))
import Filehub.SessionPool qualified as SessionPool
import Filehub.Env.Internal qualified as Env


data TargetView = TargetView
  { target :: Target
  , sessionData :: TargetSessionData
  , index :: Int
  }
  deriving (Generic)


getTargetId :: Target -> TargetId
getTargetId (S3Target t) = t.targetId
getTargetId (FileTarget t) = t.targetId


currentTarget :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es TargetView
currentTarget sessionId = do
  mSession <- SessionPool.getSession sessionId
  targets <- Env.getTargets
  maybe (throwError InvalidSession) pure do
      index <- mSession ^? #index
      targetSessionData <- mSession ^? #targets . ix index
      target <- targets ^? ix index
      pure $ TargetView target targetSessionData index


changeCurrentTarget :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> TargetId -> Eff es ()
changeCurrentTarget sessionId targetId = do
  logTrace_ [i|Changing target to #{targetId}|]
  TargetView t _ _ <- currentTarget sessionId
  targets <- Env.getTargets
  if getTargetId t == targetId
     then pure ()
     else do
       case find (\(_, x) -> getTargetId x == targetId) ([0..] `zip` targets) of
         Just (idx, _) -> do
           SessionPool.updateSession sessionId (\s -> s & #index .~ idx)
         Nothing -> do
           logAttention "Can't find target" (show targetId)
           throwError InvalidSession


withTarget :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> TargetId -> Eff es a -> Eff es a
withTarget sessionId targetId action = do
  TargetView saved _ _ <- currentTarget sessionId
  changeCurrentTarget sessionId targetId
  result <- action
  changeCurrentTarget sessionId (getTargetId saved)
  pure result


getS3Target :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es S3Target
getS3Target sessionId = do
  TargetView target _ _ <- currentTarget sessionId
  case target of
    S3Target t -> pure t
    _ -> throwError TargetError
