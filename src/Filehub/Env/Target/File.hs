module Filehub.Env.Target.File where

import Filehub.Options ( TargetOption(..), FSTargetOption(..), S3TargetOption(..) )
import Filehub.Types
    ( Target(..),
      FileTarget(..),
      S3Target(..),
      TargetId(..),
      Env(..),
      SessionId,
      Target,
      TargetSessionData )
import Data.UUID.V4 qualified as UUID
import Data.List (find)
import Data.Generics.Labels ()
import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Log (logAttention, Log)
import UnliftIO (MonadUnliftIO, MonadIO (..))
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import System.Directory (makeAbsolute)
import GHC.Generics (Generic)
import Filehub.Domain.Types (FilehubError (..))
import Filehub.Env.SessionPool qualified as SessionPool
import Filehub.Env.Internal qualified as Env
import Amazonka qualified


initTarget :: MonadUnliftIO m => FSTargetOption -> m FileTarget
initTarget to = do
  targetId <- liftIO $ TargetId <$> UUID.nextRandom
  root <- liftIO $ makeAbsolute to.root
  pure $ FileTarget_ targetId Nothing root
