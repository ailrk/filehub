module Filehub.Target.File (initTarget) where

import Filehub.Options ( FSTargetOption(..) )
import Filehub.Types
    ( FileTarget(..),
      TargetId(..) )
import Data.UUID.V4 qualified as UUID
import Data.Generics.Labels ()
import Data.String.Interpolate (i)
import UnliftIO (MonadUnliftIO, MonadIO (..))
import Lens.Micro.Platform ()
import Log (MonadLog)
import Log.Class (logInfo_)
import System.Directory (makeAbsolute)


initTarget :: (MonadUnliftIO m, MonadLog m) => FSTargetOption -> m FileTarget
initTarget to = do
  targetId <- liftIO $ TargetId <$> UUID.nextRandom
  root <- liftIO $ makeAbsolute to.root
  logInfo_ [i|Initialized: #{targetId} - FS #{root}|]
  pure $ FileTarget_ targetId Nothing root
