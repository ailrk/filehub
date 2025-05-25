{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Filehub.Server.Context.Resolution where

import Servant
import Filehub.Types (Env(..), Display (..))
import Network.Wai (Request (..))
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Filehub.Env qualified as Env
import Filehub.Cookie qualified as Cookies
import Filehub.Server.Internal (parseHeader')
import Lens.Micro ((&))
import Filehub.Error (withServerError)
import Filehub.Monad (runFilehub)
import UnliftIO (MonadIO(..))
import Data.ByteString.Lazy (ByteString)

-- | Withness
data ConfirmDesktopOnly = ConfirmDesktopOnly
data ConfirmMobilOnly = ConfirmMobilOnly

desktopOnlyHandler :: Env -> AuthHandler Request ConfirmDesktopOnly
desktopOnlyHandler =
  displayOnlyHandler
    ConfirmDesktopOnly
    (\case
      Desktop -> True
      _ -> False
    )
    "Only allowed for desktop view"


mobileOnlyHandler :: Env -> AuthHandler Request ConfirmMobilOnly
mobileOnlyHandler =
  displayOnlyHandler
    ConfirmMobilOnly
    (\case
      Mobile -> True
      _ -> False
    )
    "Only allowed for mobile view"


displayOnlyHandler :: witness -> (Display -> Bool) -> ByteString -> Env -> AuthHandler Request witness
displayOnlyHandler witness predicate msg env =
  mkAuthHandler $ \req -> do
    sessionId <- maybe (throwError $ err401 { errBody = "invalid session" }) pure do
      cookie <-  lookup "Cookie" $ requestHeaders req
      parseHeader' cookie >>= Cookies.getSessionId
    display <- liftIO . runFilehub env $ Env.getDisplay sessionId & withServerError
    case display of
      Right d | predicate d -> pure witness
      _ -> throwError err400 { errBody = msg }
