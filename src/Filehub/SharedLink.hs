module Filehub.SharedLink where

import Data.File (FileInfo)
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Effectful (Eff)


newtype SharedLinkHash = SharedLinkHash Text
  deriving (Show, Eq, Ord)


data SharedLink = SharedLink
  { file      :: FileInfo
  , hash      :: SharedLinkHash
  , createdAt :: UTCTime
  , expiresAt :: UTCTime
  }


data SharedLinkPool = SharedLinkPool
  { sharedLinks :: Map SharedLinkHash SharedLink
  }


createSharedLink :: FileInfo -> Eff es SharedLink
createSharedLink file = undefined


revokeSharedLink :: SharedLinkHash -> Eff es ()
revokeSharedLink hash = undefined


newShareLinkPool :: Eff es SharedLinkPool
newShareLinkPool = undefined
