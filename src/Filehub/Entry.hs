{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Filehub.Entry (main) where

import Effectful (runEff)
import Data.Functor ((<&>))
import Data.Time (secondsToNominalDiffTime)
import Text.Printf (printf)
import UnliftIO (SomeException, hFlush, stdout, catch)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import System.Directory (makeAbsolute)
import Filehub.Auth qualified as Auth
import Filehub.Monad
import Filehub.Options (Options(..), parseOptions)
import Filehub.Env
import Filehub.SessionPool qualified as SessionPool
import Filehub.Target qualified as Target
import Filehub.Server qualified as Server
import Filehub.Routes qualified as Routes
import Servant (serveWithContextT, Context (..), Application, serveDirectoryWebApp, (:<|>) (..))
import Paths_filehub qualified


application :: Env -> Application
application env
  = Server.sessionMiddleware env
  . serveWithContextT Routes.api ctx (toServantHandler env)
  $ server
  where
    server = Server.server
      :<|> serveDirectoryWebApp env.dataDir
      :<|> Server.dynamicRaw env

    ctx = Auth.sessionHandler env :. EmptyContext

------------------------------------
-- main
------------------------------------


main :: IO ()
main = do
  options <- parseOptions
  dataDir <- Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data")
  sessionPool <- runEff SessionPool.new
  targets <- Target.fromTargetOptions options.targets
  printf "PORT: %d\n" options.port
  let env =
        Env
          { port = options.port
          , dataDir = dataDir
          , theme = options.theme
          , sessionPool = sessionPool
          , sessionDuration = secondsToNominalDiffTime (60 * 60)
          , targets = targets
          }
  go env `catch` handler
  where
    go env = do
      putStr "[Filehub server is up and running]\n" >> hFlush stdout
      let settings = setPort env.port defaultSettings
      runSettings settings . logStdout $ application env
    handler (e :: SomeException) = putStrLn ("server is down " <> show e) >> hFlush stdout
