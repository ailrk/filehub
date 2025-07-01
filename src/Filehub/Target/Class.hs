{-# LANGUAGE TypeFamilies #-}
module Filehub.Target.Class where

import Filehub.Target.Types.TargetId (TargetId)


class IsTarget b where
  data family Backend b
  getTargetIdFromBackend :: Backend b -> TargetId
