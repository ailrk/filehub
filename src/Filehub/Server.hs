module Filehub.Server (dynamicRaw) where

import Network.Wai
import Network.Wai.Application.Static
import Effectful.Concurrent.STM (runConcurrent, writeTVar, atomically, readTVar)
import Servant (Raw, Tagged (..), ServerT)
import Effectful (runEff, MonadIO (liftIO))
import Effectful.Reader.Dynamic (runReader)
import Filehub.Env (Env)
import Filehub.Env qualified as Env
import Effectful.Error.Dynamic
    ( runErrorNoCallStack )
import Filehub.Domain (FilehubError)
import Web.Cookie (parseCookies)
import Filehub.Cookie (getSessionId)
import Network.HTTP.Types (status400)


dynamicRaw :: Env -> ServerT Raw m
dynamicRaw env = Tagged $ \req respond -> run do
  let mSessionId = do
        bytes <- lookup "Cookie" (requestHeaders req)
        getSessionId . parseCookies $ bytes
  case mSessionId of
    Just sessionId -> do
      mRoot <- runErrorNoCallStack @FilehubError . runReader env $ Env.getRoot sessionId
      case mRoot of
        Right root -> do
          path <- atomically $ do
            env.currentRoot `writeTVar` root
            readTVar env.currentRoot
          let app = staticApp (defaultWebAppSettings path)
          liftIO $ app req respond
        Left _ ->
          liftIO $ respond response400
    Nothing -> do
      liftIO $ respond response400
  where
    response400 = responseLBS status400 [("Content-Type", "text/plain")] "Invalid Session"
    run = runEff . runConcurrent
