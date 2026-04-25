module Storage.Error (StorageError(..)) where

import Text.Debug (Debug(..))
import UnliftIO.Exception (Exception)


data StorageError
  = InvalidDir  String
  | InvalidPath String
  | FileExists  String
  | TargetError String
  | CopyError   String
  | WriteError  String
  deriving (Show, Exception)


instance Debug StorageError where debug = show
