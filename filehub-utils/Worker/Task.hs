module Worker.Task (TaskId , newTaskId)
  where

import Data.Unique (Unique, hashUnique, newUnique)
import Data.Hashable (Hashable)
import Data.Aeson (ToJSON(..))
import UnliftIO (MonadIO (liftIO))
import Text.Debug (Debug(..))


newtype TaskId = TaskId Unique
  deriving (Eq, Ord)
  deriving newtype (Hashable)


instance Show TaskId where
  show (TaskId unique) = "TaskId " <> show (hashUnique unique)


instance Debug TaskId where debug = show


instance ToJSON TaskId where
  toJSON (TaskId unique) = toJSON (hashUnique unique)


newTaskId :: MonadIO m => m TaskId
newTaskId =  TaskId <$> liftIO newUnique
