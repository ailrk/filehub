{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Filehub.Entry (main, mainDev, application) where

import Data.Time (secondsToNominalDiffTime)
import Effectful (runEff)
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (runLog)
import Filehub.Env
import Filehub.Log qualified as Log
import Filehub.Monad
import Filehub.Options (Options(..), parseOptions, TargetOption (..))
import Filehub.Routes qualified as Routes
import Filehub.Server qualified as Server
import Filehub.Server.Context.ReadOnly qualified as Server.Context.ReadOnly
import Filehub.Server.Context.Resolution qualified as Server.Context.Resolution
import Filehub.Server.Context.Session qualified as Server.Context.Session
import Filehub.Server.Middleware qualified as Server.Middleware
import Filehub.Server.Middleware.Display qualified as Server.Middleware.Display
import Filehub.Server.Middleware.Session qualified as Server.Middleware.Session
import Filehub.SessionPool qualified as SessionPool
import Filehub.Target.Storage.File qualified as FS
import Filehub.Target.Storage.S3 qualified as S3
import Filehub.Types (Target(..))
import Lens.Micro
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Paths_filehub qualified
import Servant (serveWithContextT, Context (..), Application, serveDirectoryWebApp, (:<|>) (..))
import Servant.Conduit ()
import System.Directory (makeAbsolute)
import System.Environment (withArgs)
import Text.Printf (printf)
import UnliftIO (SomeException, hFlush, stdout, catch)


application :: Env -> Application
application env
  = Server.Middleware.exposeHeaders
  . Server.Middleware.Session.sessionMiddleware env
  . Server.Middleware.dedupHeadersKeepLast
  . Server.Middleware.Display.displayMiddleware env
  . serveWithContextT Routes.api ctx (toServantHandler env)
  $ server
  where
    server = Server.server
      :<|> serveDirectoryWebApp env.dataDir

    ctx = Server.Context.Session.sessionHandler env
        :. Server.Context.ReadOnly.readOnlyHandler env
        :. Server.Context.Resolution.desktopOnlyHandler env
        :. Server.Context.Resolution.mobileOnlyHandler env
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
        transform (FSTargetOption opt) = Target <$> FS.initialize opt
        transform (S3TargetOption opt) = Target <$> S3.initialize opt


-- | For developement with ghciwatch
--   Run `ghciwatch --test-ghci "Filehub.Entry.mainDev <your args for testing>"`
--   ghciwatch will watch file changes and rerun the server automatically.
mainDev :: [String] -> IO ()
mainDev args = withArgs args main
