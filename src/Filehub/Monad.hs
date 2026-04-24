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
  , IsFilehub
  )
  where


import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Filehub.Env (Env(..))
import Filehub.Error (FilehubError, toServerError)
import Servant (ServerError)


-- | The core Application monad.
-- We use a simple Newtype over ReaderT Env (ExceptT FilehubError IO).
-- This gives you all the benefits of 'effectful' (IO access, Error handling, Reader)
-- without the type-level overhead.
newtype Filehub a = Filehub
  { unFilehub :: ReaderT Env IO a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    )


-- | Discharge the Filehub stack into IO
runFilehub :: Env -> Filehub a -> IO (Either FilehubError a)
runFilehub env action = runExceptT (runReaderT (unFilehub action) env)

-- | Convenient helper to run Filehub in IO, mapping errors to Servant ServerError.
toIO :: (ServerError -> IO a) -> Env -> Filehub a -> IO a
toIO onErr env action = do
  result <- runFilehub env action
  case result of
    Left err -> onErr (toServerError err)
    Right val -> pure val
