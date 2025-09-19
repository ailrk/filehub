module Storage.Error where


data StorageError
  = InvalidDir  String
  | FileExists  String
  | TargetError String
  | CopyError   String
  | WriteError  String
