module Filehub.Target.File where

import Filehub.Target.Types.TargetId (TargetId(..))
import Filehub.Target.Class (IsTarget (..))
import Data.Text (Text)
import Effectful (IOE, (:>), Eff, MonadIO (..))
import Effectful.Log (Log, logInfo_)
import Effectful.FileSystem (FileSystem, makeAbsolute)
import Filehub.Config (FSTargetConfig(..))
import Data.String.Interpolate (i)
import Data.UUID.V4 qualified as UUID


data FileSys


instance IsTarget FileSys where
  data Backend FileSys =
    FileBackend
      { targetId   :: TargetId
      , targetName :: Maybe Text
      , root       :: FilePath
      }
  getTargetIdFromBackend f = f.targetId



initialize :: (IOE :> es, Log :> es, FileSystem :> es) => FSTargetConfig -> Eff es (Backend FileSys)
initialize opt = do
  targetId <- liftIO $ TargetId <$> UUID.nextRandom
  root <- makeAbsolute opt.root
  logInfo_ [i|Initialized: #{targetId} - FS #{root}|]
  pure $ FileBackend targetId Nothing root
