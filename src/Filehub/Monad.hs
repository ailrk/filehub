module Filehub.Monad
  ( runFilehub
  , toIO
  , Filehub
  )
  where


import Effectful.Reader.Dynamic
import Effectful (Eff, IOE, runEff)
import Effectful.Log (Log, runLog)
import Effectful.Error.Dynamic (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Concurrent
import Filehub.Env (Env(..))
import Servant (ServerError)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad ((>=>))


type Filehub = Eff [Reader Env, Log, Error ServerError, FileSystem, Concurrent, IOE]


runFilehub :: Env -> Filehub a -> IO (Either ServerError a)
runFilehub env eff =
  runEff $
    runConcurrent
  . runFileSystem
  . runErrorNoCallStack
  . runLog "filehub" env.logger env.logLevel
  . runReader env
  $ eff


toIO :: (ServerError -> IO a) -> Env -> Filehub a -> IO a
toIO onErr env eff = do
  (runExceptT >=> either onErr pure)
  . ExceptT
  . runFilehub env
  $ eff
