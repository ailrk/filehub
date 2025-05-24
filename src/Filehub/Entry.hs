{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Filehub.Entry (main) where

import Effectful (runEff)
import Data.Time (secondsToNominalDiffTime)
import Text.Printf (printf)
import UnliftIO (SomeException, hFlush, stdout, catch)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import Servant (serveWithContextT, Context (..), Application, serveDirectoryWebApp, (:<|>) (..))
import System.Directory (makeAbsolute)
import Filehub.Server.Session qualified as Server.Session
import Filehub.Server.ReadOnly qualified as Server.ReadOnly
import Filehub.Server.Resoluiton qualified as Server.Resoluiton
import Filehub.Server.Display qualified as Server.Display
import Filehub.Server qualified as Server
import Filehub.Monad
import Filehub.Options (Options(..), parseOptions, TargetOption (..))
import Filehub.Env
import Filehub.SessionPool qualified as SessionPool
import Filehub.Routes qualified as Routes
import Filehub.Log qualified as Log
import Filehub.Storage.File qualified as FS
import Filehub.Storage.S3 qualified as S3
import Lens.Micro
import Paths_filehub qualified
import Effectful.Log (runLog)
import Filehub.Types (Target(..))
import Effectful.FileSystem (runFileSystem)


application :: Env -> Application
application env
  = Server.Session.sessionMiddleware env
  . Server.Display.displayMiddleware env
  . serveWithContextT Routes.api ctx (toServantHandler env)
  $ server
  where
    server = Server.server
      :<|> serveDirectoryWebApp env.dataDir

    ctx = Server.Session.sessionHandler env
        :. Server.ReadOnly.readOnlyHandler env
        :. Server.Resoluiton.desktopOnlyHandler env
        :. Server.Resoluiton.mobileOnlyHandler env
        :. EmptyContext

------------------------------------
-- main
------------------------------------


main :: IO ()
main = Log.withColoredStdoutLogger \logger -> do
  options <- parseOptions
  dataDir <- Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data")
  sessionPool <- runEff SessionPool.new
  targets <- runEff . runLog "Targets" logger options.verbosity . runFileSystem $ fromTargetOptions options.targets
  printf "PORT: %d\n" options.port
  printf "V: %s\n" (show options.verbosity)
  let env =
        Env
          { port = options.port
          , dataDir = dataDir
          , theme = options.theme
          , sessionPool = sessionPool
          , sessionDuration = secondsToNominalDiffTime (60 * 60)
          , targets = targets
          , readOnly = options.readOnly
          , logger = logger
          , logLevel = options.verbosity
          }
  go env `catch` handler
  where
    go env = do
      putStr "[Filehub server is up and running]\n" >> hFlush stdout
      let settings = setPort env.port defaultSettings
      runSettings settings . logStdout $ application env
    handler (e :: SomeException) = putStrLn ("server is down " <> show e) >> hFlush stdout

    fromTargetOptions opts = traverse transform opts
      where
        transform (FSTargetOption opt) = FileTarget <$> FS.initialize opt
        transform (S3TargetOption opt) = S3Target <$> S3.initialize opt
