module LockRegistry.Dummy where


import LockRegistry.Key (LockKey)


withLock :: LockKey -> IO a -> IO a
withLock _ action = action
