module Filehub.Auth where

import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant (FromHttpApiData (..), errBody, err401, throwError, Handler (..))
import Network.Wai (Request (..))
import Filehub.Types (SessionId)
import Filehub.Cookie qualified as Cookie
import Data.Text.Lazy.Encoding qualified as Text
import Lens.Micro
import Filehub.Env (Env(..))
import Filehub.Error (withServerError)
import Data.Bifunctor (Bifunctor(..))
import Data.Text.Lazy qualified as Text
import Filehub.SessionPool qualified as SessionPool
import Filehub.Monad (toServantHandler)


sessionHandler :: Env -> AuthHandler Request SessionId
sessionHandler env = mkAuthHandler handler
  where
    toEither msg Nothing = Left msg
    toEither _ (Just x) = Right x

    throw401 msg = throwError $ err401 { errBody = msg }

    handler :: Request -> Handler SessionId
    handler req = do
      sessionId <- either throw401 pure do
        header <- toEither "cookie not found" $ lookup "Cookie" $ requestHeaders req
        cookie <- bimap (Text.encodeUtf8 . Text.fromStrict) id $ parseHeader header
        toEither "can't get sessionId" $ Cookie.getSessionId cookie
      _ <- toServantHandler env $ SessionPool.getSession sessionId & withServerError
      pure sessionId
