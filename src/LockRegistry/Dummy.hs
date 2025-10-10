module LockRegistry.Dummy (withLock) where


import LockRegistry.Key (LockKey)


withLock :: LockKey -> IO a -> IO a
withLock _ action = action
