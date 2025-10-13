module Filehub.Types where

import Target.Types (Target)
import Data.File (FileInfo)
import Text.Debug (Debug)


data Layout
  = ThumbnailLayout
  | ListLayout


data CopyState
  = CopySelected [(Target, [FileInfo])]
  | Paste [(Target, [FileInfo])]
  | NoCopyPaste


data Selected

instance Debug Selected
