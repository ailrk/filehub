module Storage.Error where
import Data.Text (Text)


data StorageError
  = InvalidDir  Text
  | FileExists  Text
  | TargetError Text
