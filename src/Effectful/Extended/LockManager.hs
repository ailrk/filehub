{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Effectful.Extended.LockManager
  ( LocalLockManagerT(..)
  , runLockManagerLocal
  , DummyLockManagerT(..)
  , runLockManagerDummy
  , MonadLockManager(..)
  , mkLockKey
  )
  where


import LockRegistry.Local qualified as Local
import LockRegistry.Key (LockKey, mkLockKey)
import LockRegistry.Dummy qualified as Dummy
import Control.Monad.Trans.Control (MonadTransControl (..))
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import UnliftIO (MonadIO, MonadUnliftIO (..))
import Control.Monad.Base (MonadBase)


class (Monad m) => MonadLockManager m where
  withLock :: LockKey -> m a -> m a
  withLocks :: [LockKey] -> m a -> m a

  default withLock :: (MonadTransControl t, MonadLockManager m', m ~ t m') => LockKey -> m a -> m a
  default withLocks :: (MonadTransControl t, MonadLockManager m', m ~ t m') => [LockKey] -> m a -> m a

  withLock key action = liftWith (\run -> withLock key (run action)) >>= restoreT . pure
  withLocks keys action = liftWith (\run -> withLocks keys (run action)) >>= restoreT . pure


newtype LocalLockManagerT m a = LocalLockManagerT { unLocalLockManagerT :: ReaderT Local.LockRegistry m a }
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadUnliftIO
  , MonadBase b
  , MonadReader Local.LockRegistry
  )


runLockManagerLocal :: Local.LockRegistry -> LocalLockManagerT m a -> m a
runLockManagerLocal reg (LocalLockManagerT m) = (`runReaderT` reg) m


instance MonadUnliftIO m => MonadLockManager (LocalLockManagerT m) where
  withLock key action = do
    reg <- ask
    withRunInIO \run -> do
      Local.withLock reg key (run action)
  withLocks keys action = do
    reg <- ask
    withRunInIO \run -> do
      Local.withLocks reg keys (run action)


newtype DummyLockManagerT m a = DummyLockManagerT { unLocalLockManagerT :: m a }
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadUnliftIO
  , MonadBase b
  )


runLockManagerDummy :: DummyLockManagerT m a -> m a
runLockManagerDummy (DummyLockManagerT m) = m


instance MonadUnliftIO m => MonadLockManager (DummyLockManagerT m) where
  withLock key action = withRunInIO \run -> Dummy.withLocks [key] (run action)
  withLocks keys action = withRunInIO \run -> Dummy.withLocks keys (run action)
