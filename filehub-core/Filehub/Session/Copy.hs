{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Session.Copy
  ( selectFiles
  , cutFiles
  , clearCopyState
  , CopyState
  )
  where

import Filehub.Prelude
import Control.Handle.Storage (Storage(..))
import Data.ClientPath (Root(..), AbsPath(..), ClientPath(..))
import Data.ClientPath qualified as ClientPath
import Filehub.Monad (Filehub)
import Target.File (FileSys, Target(..))
import Target.S3 (S3)
import Target.Types (handleTarget, targetHandler)
import Filehub.Session (Session(..), SessionId, CopyState(..), TargetView(..), makeStorage, getTarget, getCurrentTarget, fromClientPathChecked, modifySession, withSession_, withSession)
import Control.Monad.Reader (MonadReader(ask))


clearCopyState :: SessionId -> Filehub ()
clearCopyState sessionId = modifySession sessionId \s -> pure s { copyState = NoCopyPaste }


selectFiles :: SessionId -> [ClientPath] -> Filehub ()
selectFiles sessionId clientPaths = do
  env <- ask
  (target, root) <- withSession sessionId \s -> do
    TargetView t _ <- getCurrentTarget env s
    let targetRoot = fromMaybe (Root (AbsPath "")) $ handleTarget t
          [ targetHandler @FileSys \(FileBackend { root }) -> root
          , targetHandler @S3      \_ -> Root (AbsPath "")
          ]
    pure (t, targetRoot)
  paths      <- traverse (fromClientPathChecked root) clientPaths
  targetView <- withSession sessionId \s -> getTarget env s target
  storage    <- makeStorage targetView
  files      <- traverse storage.get paths
  withSession_ sessionId \s ->
    pure (s { copyState = Paste [(target, files)] }, ())


cutFiles :: SessionId -> [ClientPath] -> Filehub ()
cutFiles sessionId clientPaths = do
  env <- ask
  (target, root) <- withSession sessionId \s -> do
    TargetView t _ <- getCurrentTarget env s
    let targetRoot = fromMaybe (Root (AbsPath "")) $ handleTarget t
          [ targetHandler @FileSys \(FileBackend { root }) -> root
          , targetHandler @S3      \_ -> Root (AbsPath "")
          ]
    pure (t, targetRoot)
  paths      <- traverse (fromClientPathChecked root) clientPaths
  targetView <- withSession sessionId \s -> getTarget env s target
  storage    <- makeStorage targetView
  files      <- traverse storage.get paths
  withSession_ sessionId \s ->
    pure (s { copyState = Cut [(target, files)] }, ())
