{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Filehub.Auth.Types where


import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Hashable (Hashable)
import Effectful (Eff, (:>), MonadIO (..), IOE)
import Prelude hiding (readFile)

import Data.Time (UTCTime)
import Data.Map.Strict (Map)
import Filehub.Session.Types.SessionId (SessionId)
import Filehub.Auth.OIDC qualified as Auth.OIDC
import Filehub.Auth.Simple qualified as Auth.Simple


data ActiveUser = ActiveUser
  { authId :: AuthId
  , loginAt :: UTCTime
  , sessions :: [SessionId]
  , auth :: Auth
  }
  deriving (Show, Eq)


data Auth
  = Simple Auth.Simple.Username
  | OIDC Auth.OIDC.User
  deriving (Show, Eq)


newtype AuthId = AuthId UUID deriving (Show, Eq, Ord, Hashable)

newtype ActiveUsers = ActiveUsers (Map AuthId ActiveUser)
  deriving (Show, Eq)


createAuthId :: (IOE :> es) => Eff es AuthId
createAuthId = AuthId <$> liftIO UUID.nextRandom
