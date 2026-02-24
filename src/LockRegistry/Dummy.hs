module LockRegistry.Dummy (withLocks) where


import LockRegistry.Key (LockKey)


withLocks :: [LockKey] -> IO a -> IO a
withLocks _ action = action
