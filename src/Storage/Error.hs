module Storage.Error where


data StorageError
  = InvalidDir  String
  | InvalidPath String
  | FileExists  String
  | TargetError String
  | CopyError   String
  | WriteError  String
