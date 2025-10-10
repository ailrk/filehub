{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.Session.Types.SessionId (SessionId(..)) where

import Data.UUID (UUID)
import Data.Hashable (Hashable)


newtype SessionId = SessionId UUID
  deriving (Show, Eq, Ord, Hashable)
