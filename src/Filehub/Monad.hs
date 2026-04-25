{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
import UnliftIO (try, MonadUnliftIO (..), readIORef, atomicModifyIORef')
import Servant (ServerError)
import Log (MonadLog(..), LogT, runLogT)
import Control.Extended.Cache (MonadCache (..))
import Control.Extended.LockManager (MonadLockManager (..))
import LockRegistry.Local qualified as Local
import Cache.InMemory (InMemoryCache(..))
import Data.Time (getCurrentTime)
import Cache.InMemory qualified as InMemory
import Control.Monad (void)


-- | The core Application monad.
newtype Filehub a = Filehub
  { unFilehub :: ReaderT Env (LogT IO) a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader Env
    , MonadLog
    )


instance MonadCache Filehub where
  cacheLookup key = do
    InMemoryCache cacheRef <- asks (.cache)
    cache <- liftIO $ readIORef cacheRef
    now <- liftIO getCurrentTime
    case InMemory.lookup now key cache of
      Just (value, cache') -> do
        atomicModifyIORef' cacheRef (const (cache', ()))
        pure value
      Nothing -> pure Nothing
  cacheInsert key mDeps mTTL value = do
    InMemoryCache cacheRef <- asks (.cache)
    now <- liftIO getCurrentTime
    void $ atomicModifyIORef' cacheRef (\cache -> (InMemory.insert now key mDeps mTTL value cache, ()))
  cacheDelete key = do
    InMemoryCache cacheRef <- asks (.cache)
    void $ atomicModifyIORef' cacheRef (\cache -> (InMemory.delete key cache, ()))
  cacheFlush = do
    InMemoryCache cacheRef <- asks (.cache)
    void $ atomicModifyIORef' cacheRef (\cache -> (InMemory.empty cache.capacity, ()))



instance MonadLockManager Filehub where
  withLock key action = do
    reg <- asks (.lockRegistry)
    withRunInIO \run -> do
      Local.withLock reg key (run action)
  withLocks keys action = do
    reg <- asks (.lockRegistry)
    withRunInIO \run -> do
      Local.withLocks reg keys (run action)



-- | Discharge the Filehub stack into IO
runFilehub :: Env -> Filehub a -> IO (Either FilehubError a)
runFilehub env action = try
                      . runLogT "filehub" env.logger env.logLevel
                      . (`runReaderT` env)
                      . (.unFilehub)
                      $ action


-- | Convenient helper to run Filehub in IO, mapping errors to Servant ServerError.
toIO :: (ServerError -> IO a) -> Env -> Filehub a -> IO a
toIO onErr env action = do
  result <- runFilehub env action
  case result of
    Left err -> onErr (toServerError err)
    Right val -> pure val
