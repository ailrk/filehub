module Filehub.ActiveUser.Pool
  ( new
  , add
  , delete
  )
  where

import Filehub.ActiveUser.Types qualified as ActiveUser
import Filehub.ActiveUser.Types (ActiveUser(..))
import Data.HashTable.IO qualified as HashTable
import Filehub.Env
import Filehub.Auth.Types (AuthId)
import UnliftIO (MonadIO (..))
import Filehub.Monad (Filehub)
import Control.Monad.Reader (asks)


new :: MonadIO m => m ActiveUser.Pool
new = do
  table <- liftIO HashTable.new
  pure $ ActiveUser.Pool table


add :: ActiveUser -> Filehub ()
add activeUser = do
  ActiveUser.Pool pool <- asks @Env (.activeUsers)
  liftIO $ HashTable.insert pool activeUser.authId activeUser


delete :: AuthId -> Filehub ()
delete authId = do
  ActiveUser.Pool pool <- asks @Env (.activeUsers)
  liftIO $ HashTable.delete pool authId
