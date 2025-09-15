module Filehub.LockRegistry.Dummy where


import Filehub.LockRegistry.Key (LockKey)


withLock :: LockKey -> IO a -> IO a
withLock _ action = action
