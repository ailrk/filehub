{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Text (Text)
import Text.Printf (printf)
import UnliftIO (SomeException, hFlush, stdout, catch)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Handler.Warp (setPort, defaultSettings, runSettings)
import Servant ((:>), Get, PlainText, serveWithContextT, Context (..), NamedRoutes, Application, (:-), Raw, serveDirectoryWebApp, (:<|>) (..))
import Data.Data (Proxy(..))
import System.Directory (makeAbsolute)
import Filehub.Options (Options(..), parseOptions)
import Filehub.Env
import Filehub
import Filehub.Index qualified as Index
import GHC.Generics (Generic)
import Filehub.Domain (getFile, loadDirContents, FilehubError, SortFileBy (..))
import Effectful (runEff)
import Effectful.Error.Dynamic (runErrorNoCallStack)
import Effectful.FileSystem (runFileSystem)
import Paths_filehub qualified
import Data.Functor ((<&>))
import UnliftIO.STM (newTVarIO)


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
  currentDir <- newTVarIO root
  sortFileBy <- newTVarIO ByName

  dir <- do
    eFile <- runEff . runFileSystem . runErrorNoCallStack @FilehubError $ do
      getFile root >>= loadDirContents
    case eFile of
      Left err -> fail (show err)
      Right d -> newTVarIO d

  dataDir <- Paths_filehub.getDataDir >>= makeAbsolute  <&> (++ "/data")

  printf "PORT: %d\n" options.port
  let env =
        Env
          { port = options.port
          , root = root
          , rootTree = dir
          , currentDir = currentDir
          , sortFileBy = sortFileBy
          , dataDir = dataDir
          , theme = options.theme
          }
  go env `catch` handler
  where
    go env = do
      putStr "[Filehub server is up and running]\n" >> hFlush stdout
      let settings = setPort env.port defaultSettings
      runSettings settings . logStdout $ application env
    handler (e :: SomeException) = putStrLn ("server is down " <> show e) >> hFlush stdout
