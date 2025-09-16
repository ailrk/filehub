{-# LANGUAGE TypeFamilies #-}
module Target.Class where

import Target.Types.TargetId (TargetId)


class IsTarget b where
  data family Backend b
  getTargetIdFromBackend :: Backend b -> TargetId
