module Filehub.Monad
  ( toServantHandler
  , runFilehub
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
import Servant (Handler (..), ServerError)
import Control.Monad.Trans.Except (ExceptT(ExceptT))


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


toServantHandler :: Env -> Filehub a -> Handler a
toServantHandler env eff =
  Handler
  . ExceptT
  . runFilehub env
  $ eff
