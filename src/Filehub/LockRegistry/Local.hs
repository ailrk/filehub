{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Filehub.LockRegistry.Local
  ( new
  , withLock
  , LockRegistry(..)
  )
  where

import UnliftIO.STM
  (TVar, TMVar, newTVarIO, atomically, readTVar, newTMVar, writeTVar, takeTMVar, putTMVar)
import Data.Map.Strict qualified  as M
import UnliftIO (finally)
import Filehub.LockRegistry.Key (LockKey)


-- | A global lock registry
newtype LockRegistry = LockRegistry (TVar (M.Map LockKey (TMVar ())))


new :: IO LockRegistry
new = LockRegistry <$> newTVarIO M.empty


withLock :: LockRegistry -> LockKey -> IO a -> IO a
withLock (LockRegistry tv) key action = do
  lock <- atomically $ do
    m <- readTVar tv
    case M.lookup key m of
      Just tmv -> return tmv
      Nothing -> do
        tmv <- newTMVar ()
        writeTVar tv (M.insert key tmv m)
        pure tmv
  atomically $ takeTMVar lock
  result <- action `finally` (atomically $ putTMVar lock ())
  pure result
