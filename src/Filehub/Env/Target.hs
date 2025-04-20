{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
module Filehub.Env.Target
  ( TargetView(..)
  , fromTargetOptions
  , getTargetId
  , currentTarget
  , changeCurrentTarget
  , getS3Target
  ) where

import Filehub.Options ( TargetOption(..) )
import Filehub.Types
    ( Target(..),
      FileTarget(..),
      S3Target(..),
      TargetId(..),
      Env(..),
      SessionId,
      Target,
      TargetSessionData )
import Data.List (find)
import Data.Generics.Labels ()
import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Log (logAttention, Log)
import UnliftIO (MonadUnliftIO)
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import GHC.Generics (Generic)
import Filehub.Error (FilehubError (..))
import Filehub.Env.SessionPool qualified as SessionPool
import Filehub.Env.Internal qualified as Env
import Filehub.Env.Target.File qualified as Env.File
import Filehub.Env.Target.S3 qualified as Env.S3


data TargetView = TargetView
  { target :: Target
  , sessionData :: TargetSessionData
  , index :: Int
  }
  deriving (Generic)


getTargetId :: Target -> TargetId
getTargetId (S3Target t) = t.targetId
getTargetId (FileTarget t) = t.targetId


fromTargetOptions :: MonadUnliftIO m => [TargetOption] -> m [Target]
fromTargetOptions tos = traverse transform tos
  where
    transform (FSTargetOption to) = FileTarget <$> Env.File.initTarget to
    transform (S3TargetOption to) = S3Target <$> Env.S3.initTarget to


currentTarget :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Eff es TargetView
currentTarget sessionId = do
  mSession <- SessionPool.getSession sessionId
  targets <- Env.getTargets
  maybe (throwError InvalidSession) pure do
      index <- mSession ^? _Just . #index
      targetSessionData <- mSession ^? _Just . #targets . ix index
      target <- targets ^? ix index
      pure $ TargetView target targetSessionData index


changeCurrentTarget :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> TargetId -> Eff es ()
changeCurrentTarget sessionId targetId = do
  TargetView t _ _ <- currentTarget sessionId
  targets <- Env.getTargets
  if getTargetId t == targetId
     then pure ()
     else do
       case find (\(_, x) -> getTargetId x == targetId) ([0..] `zip` targets) of
         Just (idx, _) -> do
           SessionPool.updateSession sessionId (\s -> s & #index .~ idx)
         Nothing -> do
           logAttention "[changeCurrentTarget] can't find target" (show targetId)
           throwError InvalidSession


getS3Target :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Eff es S3Target
getS3Target sessionId = do
  TargetView target _ _ <- currentTarget sessionId
  case target of
    S3Target t -> pure t
    _ -> throwError TargetError
