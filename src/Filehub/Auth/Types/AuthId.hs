{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Auth.Types.AuthId (AuthId(..), createAuthId) where

import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Hashable (Hashable)
import Effectful (Eff, (:>), MonadIO (..), IOE)
import Prelude hiding (readFile)


newtype AuthId = AuthId UUID
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable)


createAuthId :: (IOE :> es) => Eff es AuthId
createAuthId = AuthId <$> liftIO UUID.nextRandom
