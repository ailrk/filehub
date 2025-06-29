module Filehub.Copy.Types (CopyState(..)) where

import Filehub.Target.Types (Target)
import Filehub.File (File)


data CopyState
 -- | Ready to paste
  = CopySelected [(Target, [File])]
  -- | Start pasting files to target path
  | Paste [(Target, [File])]
  -- | No copy paste action being performed at the moment.
  | NoCopyPaste
