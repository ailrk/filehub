{-# LANGUAGE NamedFieldPuns #-}
module Target.File where

import Filehub.Prelude
import Target.Types (TargetId(..), Target, IsTarget(..), HasTargetId(..))
import Data.UUID.V4 qualified as UUID
import Text.Debug (Debug(..))
import Data.ClientPath (AbsPath (..), Root (..))
import UnliftIO.Directory (makeAbsolute)
import Log (logInfo_, MonadLog)


data FileSys


instance Debug (Target FileSys) where
  debug FileBackend { targetId, targetName, root} =
    mconcat
      [ "[<FileBackend>, "
      , debug targetId, ", "
      , show targetName, ", "
      , coerce root
      , "]"
      ]


instance HasTargetId (Target FileSys) where
  getTargetId FileBackend { targetId } = targetId


instance IsTarget FileSys where
  data instance Target FileSys =
    FileBackend
      { targetId   :: TargetId
      , targetName :: Maybe Text
      , root       :: Root
      }

  data instance Config FileSys = Config
    { root :: FilePath
    }
    deriving (Show, Eq)


instance Debug (Config FileSys) where debug = show


initialize :: (MonadLog m, MonadIO m) => Config FileSys -> m (Target FileSys)
initialize opt = do
  targetId <- liftIO $ TargetId <$> UUID.nextRandom
  root     <- (Root . AbsPath) <$> makeAbsolute opt.root
  logInfo_ [i|[99s5fd] Initialized: #{targetId} - FS #{root}|]
  pure $ FileBackend targetId Nothing root
