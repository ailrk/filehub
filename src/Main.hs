{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Effectful (runEff)
import Data.Data (Proxy(..))
import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Time (secondsToNominalDiffTime)
import Text.Printf (printf)
import UnliftIO (SomeException, hFlush, stdout, catch)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import System.Directory (makeAbsolute)
import Filehub.Options (Options(..), parseOptions)
import Filehub.Env
import Filehub
import Filehub.Index qualified as Index
import Filehub.SessionPool qualified as SessionPool
import GHC.Generics (Generic)
import Servant ((:>), Get, PlainText, serveWithContextT, Context (..), NamedRoutes, Application, (:-), Raw, serveDirectoryWebApp, (:<|>) (..))
import Paths_filehub qualified


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
      :<|> serveDirectoryWebApp env.root


healthz :: Filehub Text
healthz = pure "ok"


------------------------------------
-- main
------------------------------------


main :: IO ()
main = do
  options <- parseOptions
  root <- makeAbsolute options.root

  dataDir <- Paths_filehub.getDataDir >>= makeAbsolute  <&> (++ "/data")

  sessionPool <- runEff SessionPool.new

  printf "PORT: %d\n" options.port
  let env =
        Env
          { port = options.port
          , root = root
          , dataDir = dataDir
          , theme = options.theme
          , sessionPool = sessionPool
          , sessionDuration = secondsToNominalDiffTime (60 * 60)
          }
  go env `catch` handler
  where
    go env = do
      putStr "[Filehub server is up and running]\n" >> hFlush stdout
      let settings = setPort env.port defaultSettings
      runSettings settings . logStdout $ application env
    handler (e :: SomeException) = putStrLn ("server is down " <> show e) >> hFlush stdout
