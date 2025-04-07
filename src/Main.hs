{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Effectful (runEff)
import Data.Data (Proxy(..))
import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Time (secondsToNominalDiffTime)
import Text.Printf (printf)
import UnliftIO (SomeException, hFlush, stdout, catch, newTVarIO)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import System.Directory (makeAbsolute)
import Filehub
import Filehub.Options (Options(..), parseOptions)
import Filehub.Env
import Filehub.Index qualified as Index
import Filehub.Env.SessionPool qualified as SessionPool
import Filehub.Env.Target qualified as Target
import Filehub.Server (dynamicRaw)
import GHC.Generics (Generic)
import Servant ((:>), Get, PlainText, serveWithContextT, Context (..), NamedRoutes, Application, (:-), Raw, serveDirectoryWebApp, (:<|>) (..))
import Paths_filehub qualified
import Filehub.Types (Target(..), FileTarget(..))


data Api mode = Api
  { index :: mode :- NamedRoutes Index.Api
  , healthz :: mode :- "healthz" :> Get '[PlainText] Text
  }
  deriving Generic


type API = NamedRoutes Api
      :<|> "static" :> Raw -- static files for the app
      :<|> Raw -- direct access of the underlying directory


application :: Env -> Application
application env = serveWithContextT (Proxy @API) EmptyContext (toServantHandler env) server
  where
    server = Api
      { index = Index.server
      , healthz = healthz
      }
      :<|> serveDirectoryWebApp env.dataDir
      :<|> dynamicRaw env


healthz :: Filehub Text
healthz = pure "ok"


------------------------------------
-- main
------------------------------------


main :: IO ()
main = do
  options <- parseOptions
  dataDir <- Paths_filehub.getDataDir >>= makeAbsolute <&> (++ "/data")
  sessionPool <- runEff SessionPool.new
  targets <- Target.fromTargetOptions options.targets
  currentRoot <- newTVarIO $ do
    case head targets of
      FileTarget t -> t.root
      S3Target _ -> "/"
  printf "PORT: %d\n" options.port
  let env =
        Env
          { port = options.port
          , dataDir = dataDir
          , theme = options.theme
          , sessionPool = sessionPool
          , sessionDuration = secondsToNominalDiffTime (60 * 60)
          , targets = targets
          , currentRoot = currentRoot
          }
  go env `catch` handler
  where
    go env = do
      putStr "[Filehub server is up and running]\n" >> hFlush stdout
      let settings = setPort env.port defaultSettings
      runSettings settings . logStdout $ application env
    handler (e :: SomeException) = putStrLn ("server is down " <> show e) >> hFlush stdout
