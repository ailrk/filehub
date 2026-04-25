module Filehub.Auth.Types.AuthId (AuthId(..), createAuthId) where

import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Hashable (Hashable)
import Prelude hiding (readFile)
import UnliftIO (MonadIO (..))


newtype AuthId = AuthId UUID
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable)


createAuthId :: MonadIO m => m AuthId
createAuthId = AuthId <$> liftIO UUID.nextRandom
