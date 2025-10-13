module Storage.Error (StorageError(..)) where

import Text.Debug (Debug(..))


data StorageError
  = InvalidDir  String
  | InvalidPath String
  | FileExists  String
  | TargetError String
  | CopyError   String
  | WriteError  String
  deriving (Show)


instance Debug StorageError where debug = show
