{-# LANGUAGE ConstraintKinds #-}
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
import UnliftIO (try, MonadUnliftIO)
import Servant (ServerError)
import Log (MonadLog(..))


-- | The core Application monad.
newtype Filehub a = Filehub
  { unFilehub :: ReaderT Env IO a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader Env
    )


instance MonadLog Filehub where
  logMessage = undefined
  localData = undefined
  localDomain = undefined
  localMaxLogLevel = undefined
  getLoggerEnv = undefined


-- | Discharge the Filehub stack into IO
runFilehub :: Env -> Filehub a -> IO (Either FilehubError a)
runFilehub env action = try (runReaderT (action.unFilehub) env)

-- | Convenient helper to run Filehub in IO, mapping errors to Servant ServerError.
toIO :: (ServerError -> IO a) -> Env -> Filehub a -> IO a
toIO onErr env action = do
  result <- runFilehub env action
  case result of
    Left err -> onErr (toServerError err)
    Right val -> pure val
