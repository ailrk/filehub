module Filehub.ActiveUser.Pool
  ( new
  , add
  , delete
  )
  where

import Filehub.ActiveUser.Types qualified as ActiveUser
import Filehub.ActiveUser.Types (ActiveUser(..))
import Data.HashTable.IO qualified as HashTable
import Effectful (IOE, (:>), Eff, MonadIO (..))
import Effectful.Reader.Dynamic (Reader, asks)
import Filehub.Env
import Filehub.Auth.Types (AuthId)


new :: (IOE :> es) => Eff es ActiveUser.Pool
new = do
  table <- liftIO HashTable.new
  pure $ ActiveUser.Pool table


add :: (Reader Env :> es, IOE :> es) => ActiveUser -> Eff es ()
add activeUser = do
  ActiveUser.Pool pool <- asks @Env (.activeUsers)
  liftIO $ HashTable.insert pool activeUser.authId activeUser


delete :: (Reader Env :> es, IOE :> es) => AuthId -> Eff es ()
delete authId = do
  ActiveUser.Pool pool <- asks @Env (.activeUsers)
  liftIO $ HashTable.delete pool authId
