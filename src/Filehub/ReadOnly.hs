{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Filehub.ReadOnly where
import Servant
import Servant.Server
import Filehub.Types (Env(..))
import Servant.API ((:>))
import Data.Data (Proxy(..))
import Network.Wai (responseLBS, Request)
import Network.HTTP.Types (status400)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)


readOnlyHandler :: Env -> AuthHandler Request ()
readOnlyHandler env =
  mkAuthHandler $ \_ ->
    if env.readOnly
      then throwError err400 { errBody = "Read-only mode is enabled" }
      else return ()
