module Filehub.Env.Target.File (initTarget) where

import Filehub.Options ( FSTargetOption(..) )
import Filehub.Types
    ( FileTarget(..),
      TargetId(..) )
import Data.UUID.V4 qualified as UUID
import Data.Generics.Labels ()
import UnliftIO (MonadUnliftIO, MonadIO (..))
import Lens.Micro.Platform ()
import System.Directory (makeAbsolute)


initTarget :: MonadUnliftIO m => FSTargetOption -> m FileTarget
initTarget to = do
  targetId <- liftIO $ TargetId <$> UUID.nextRandom
  root <- liftIO $ makeAbsolute to.root
  pure $ FileTarget_ targetId Nothing root
