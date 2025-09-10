{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Filehub.Auth.Types where

import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Hashable (Hashable)
import Effectful (Eff, (:>), MonadIO (..), IOE)
import Prelude hiding (readFile)
import Data.Time (UTCTime)
import Filehub.Session.Types.SessionId (SessionId)
import Filehub.Auth.Simple qualified as Auth.Simple
import {-# SOURCE #-} Filehub.Auth.OIDC qualified as Auth.OIDC
import Data.Map.Strict (Map)


data ActiveUser = ActiveUser
  { authId   :: AuthId
  , loginAt  :: UTCTime
  , sessions :: [SessionId]
  , auth     :: Auth
  }
  deriving (Show, Eq)


data Auth
  = Simple Auth.Simple.Username
  | OIDC Auth.OIDC.User
  deriving (Show, Eq)



newtype ActiveUsers = ActiveUsers (Map AuthId ActiveUser)
  deriving (Show, Eq)

newtype AuthId = AuthId UUID deriving (Show, Eq, Ord, Hashable)

createAuthId :: (IOE :> es) => Eff es AuthId
createAuthId = AuthId <$> liftIO UUID.nextRandom
