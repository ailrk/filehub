module Filehub.Types where

import Target.Types (AnyTarget)
import Data.File (FileInfo)
import Text.Debug (Debug)


data Layout
  = ThumbnailLayout
  | ListLayout


data CopyState
  = CopySelected [(AnyTarget, [FileInfo])]
  | Paste [(AnyTarget, [FileInfo])]
  | NoCopyPaste


data Selected

instance Debug Selected
