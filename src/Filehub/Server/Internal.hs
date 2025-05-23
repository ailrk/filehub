{-# LANGUAGE PartialTypeSignatures #-}
module Filehub.Server.Internal where

import Effectful ( Eff, (:>), IOE )
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Env (Env (..))
import Filehub.Selected qualified as Selected
import Filehub.Types
    ( SessionId(..),
      SessionId(..))
import Filehub.Copy qualified as Copy
import Lens.Micro.Platform ()
import Prelude hiding (readFile)
import Prelude hiding (readFile)
import Servant ( ServerError(..), ServerError, FromHttpApiData (..) )
import Servant.Server (err400)
import Data.ByteString (ByteString)


-- | Ensure a query parameter presents, otherwise it's a client error
withQueryParam :: (Error ServerError :> es) => Maybe a -> Eff es a
withQueryParam m =
  case m of
    Just a -> pure a
    Nothing -> throwError err400


-- | Completely reset all state machines. This should be the only place to reset state.
clear :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clear sessionId = do
  Selected.clearSelectedAllTargets sessionId
  Copy.clearCopyState sessionId


parseHeader' :: FromHttpApiData a => ByteString -> Maybe a
parseHeader' x = either (const Nothing) Just $ parseHeader x
