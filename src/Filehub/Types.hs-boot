module Filehub.Types where

import {-# SOURCE #-} Filehub.Target.Types (Target)
import Data.File (File)


data Layout
  = ThumbnailLayout
  | ListLayout


data CopyState
  = CopySelected [(Target, [File])]
  | Paste [(Target, [File])]
  | NoCopyPaste


data Selected
