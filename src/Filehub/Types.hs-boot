module Filehub.Types where

import Target.Types (Target)
import Data.File (FileInfo)


data Layout
  = ThumbnailLayout
  | ListLayout


data CopyState
  = CopySelected [(Target, [FileInfo])]
  | Paste [(Target, [FileInfo])]
  | NoCopyPaste


data Selected
