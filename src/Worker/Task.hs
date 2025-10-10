{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Worker.Task (TaskId , newTaskId)
  where

import Data.Unique (Unique, hashUnique, newUnique)
import Data.Hashable (Hashable)
import Data.Aeson (ToJSON(..))
import UnliftIO (MonadIO (liftIO))


newtype TaskId = TaskId Unique
  deriving (Eq, Ord, Hashable)


instance Show TaskId where
  show (TaskId unique) = "TaskId " <> show (hashUnique unique)


instance ToJSON TaskId where
  toJSON (TaskId unique) = toJSON (hashUnique unique)


newTaskId :: MonadIO m => m TaskId
newTaskId =  TaskId <$> liftIO newUnique
