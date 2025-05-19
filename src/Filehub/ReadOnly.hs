{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Filehub.ReadOnly where
import Servant
import Filehub.Types (Env(..))
import Network.Wai (Request)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)


readOnlyHandler :: Env -> AuthHandler Request ()
readOnlyHandler env =
  mkAuthHandler $ \_ ->
    if env.readOnly
      then throwError err400 { errBody = "Read-only mode is enabled" }
      else return ()
