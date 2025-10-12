{-# LANGUAGE NamedFieldPuns #-}
module Target.File where

import Target.Types (TargetId(..), TargetBackend, IsTarget(..))
import Data.Text (Text)
import Effectful (IOE, (:>), Eff, MonadIO (..))
import Effectful.Log (Log, logInfo_)
import Effectful.FileSystem (FileSystem, makeAbsolute)
import Data.String.Interpolate (i)
import Data.UUID.V4 qualified as UUID


data FileSys


data instance TargetBackend FileSys =
  FileBackend
    { targetId   :: TargetId
    , targetName :: Maybe Text
    , root       :: FilePath
    }


instance IsTarget FileSys where
  getTargetIdFromBackend FileBackend { targetId } = targetId


data Config = Config
  { root :: FilePath
  }
  deriving (Show, Eq)


initialize :: (IOE :> es, Log :> es, FileSystem :> es) => Config -> Eff es (TargetBackend FileSys)
initialize opt = do
  targetId <- liftIO $ TargetId <$> UUID.nextRandom
  root     <- makeAbsolute opt.root
  logInfo_ [i|[99s5fd] Initialized: #{targetId} - FS #{root}|]
  pure $ FileBackend targetId Nothing root
