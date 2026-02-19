{-# LANGUAGE NamedFieldPuns #-}
module Target.File where

import Target.Types (TargetId(..), TargetBackend, IsTarget(..))
import Data.Text (Text)
import Effectful (IOE, (:>), Eff, MonadIO (..))
import Effectful.Log (Log, logInfo_)
import Effectful.FileSystem (FileSystem, makeAbsolute)
import Data.String.Interpolate (i)
import Data.UUID.V4 qualified as UUID
import Text.Debug (Debug(..))
import Data.ClientPath (AbsPath (..))
import Data.Coerce (coerce)


data FileSys



instance Debug (TargetBackend FileSys) where
  debug FileBackend { targetId, targetName, root} =
    mconcat
      [ "[<FileBackend>, "
      , debug targetId, ", "
      , show targetName, ", "
      , coerce root
      , "]"
      ]


instance IsTarget FileSys where
  data instance TargetBackend FileSys =
    FileBackend
      { targetId   :: TargetId
      , targetName :: Maybe Text
      , root       :: AbsPath
      }

  data instance Config FileSys = Config
    { root :: FilePath
    }
    deriving (Show, Eq)

  getTargetIdFromBackend FileBackend { targetId } = targetId



instance Debug (Config FileSys) where debug = show


initialize :: (IOE :> es, Log :> es, FileSystem :> es) => Config FileSys -> Eff es (TargetBackend FileSys)
initialize opt = do
  targetId <- liftIO $ TargetId <$> UUID.nextRandom
  root     <- AbsPath <$> makeAbsolute opt.root
  logInfo_ [i|[99s5fd] Initialized: #{targetId} - FS #{root}|]
  pure $ FileBackend targetId Nothing root
