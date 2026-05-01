{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- The effect of filehub.
module Filehub.Monad
  ( runFilehub
  , toIO
  , Filehub
  )
  where

import Control.Monad.Reader
import Filehub.Env (Env(..))
import Filehub.Error (FilehubError, toServerError)
import UnliftIO (try, MonadUnliftIO (..))
import Servant (ServerError)
import Log (MonadLog(..), LogT, runLogT)
import Control.Service.Cache (MonadCache (..))
import Control.Service.LockManager (MonadLockManager (..))
import Control.Handle.Cache (Cache(..))
import Control.Handle.LockManager (LockManager(..))
import Prelude hiding (lookup)
import Control.Monad.Trans.Resource (MonadResource (..), ResourceT, runResourceT)
import Control.Monad.Base (MonadBase (..))

-- | The core Application monad.
--
-- `Filehub` is a concrete reader monad and all capabilities including handles
-- and caches are based on `Env`. To test, swap `Env` with a stubbed one.
newtype Filehub a = Filehub
  { unFilehub :: ReaderT Env (LogT (ResourceT IO)) a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader Env
    , MonadLog
    )


instance MonadBase b m => MonadBase b (ResourceT m) where
    liftBase = lift . liftBase
    {-# INLINE liftBase #-}


instance MonadResource m => MonadResource (LogT m) where
  liftResourceT = lift . liftResourceT
  {-# INLINE liftResourceT #-}


instance MonadResource Filehub where
    liftResourceT = Filehub . lift . lift . liftResourceT
    {-# INLINE liftResourceT #-}


instance MonadCache Filehub where
  cacheLookup key = do
    Cache{lookup} <- asks (.cache)
    liftIO $ lookup key
  {-# INLINE cacheLookup #-}

  cacheInsert key mDeps mTTL value = do
    Cache{insert} <- asks (.cache)
    liftIO $ insert key mDeps mTTL value
  {-# INLINE cacheInsert #-}

  cacheDelete key = do
    cache <- asks (.cache)
    liftIO $ cache.delete key
  {-# INLINE cacheDelete #-}

  cacheFlush = do
    cache <- asks (.cache)
    liftIO $ cache.flush
  {-# INLINE cacheFlush #-}


instance MonadLockManager Filehub where
  withLock key action = do
    LockManager {withLock = withLock'} <- asks (.lockManager)
    withRunInIO \run -> do
      withLock' key (run action)
  {-# INLINE withLock #-}

  withLocks keys action = do
    LockManager {withLocks = withLocks'} <- asks (.lockManager)
    withRunInIO \run -> do
      withLocks' keys (run action)
  {-# INLINE withLocks #-}


-- | Discharge the Filehub stack into IO
runFilehub :: Env -> Filehub a -> IO (Either FilehubError a)
runFilehub env action = try
                      . runResourceT
                      . runLogT "filehub" env.logger env.logLevel
                      . (`runReaderT` env)
                      . (.unFilehub)
                      $ action
{-# INLINE runFilehub #-}

-- | Convenient helper to run Filehub in IO, mapping errors to Servant ServerError.
toIO :: (ServerError -> IO a) -> Env -> Filehub a -> IO a
toIO onErr env action = do
  result <- runFilehub env action
  case result of
    Left err -> onErr (toServerError err)
    Right val -> pure val
{-# INLINE toIO #-}
