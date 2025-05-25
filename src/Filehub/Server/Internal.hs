{-# LANGUAGE PartialTypeSignatures #-}
module Filehub.Server.Internal where

import Effectful ( Eff, (:>), IOE, MonadUnliftIO (..) )
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
import Servant ( ServerError(..), ServerError, FromHttpApiData (..), err500 )
import Servant.Server (err400)
import Data.ByteString (ByteString)
import Filehub.Monad (Filehub)
import Control.Exception (SomeException, catch)
import Data.String.Interpolate (i)
import Filehub.Error (withServerError)
import Lens.Micro ((&))


copy :: SessionId -> Filehub ()
copy sessionId = withServerError do
  Copy.select sessionId
  Copy.copy sessionId


paste :: SessionId -> Filehub ()
paste sessionId = do
  withRunInIO $ \unlift -> do
    unlift (Copy.paste sessionId & withServerError) `catch` \(_ :: SomeException) -> unlift do
      throwError (err500 { errBody = [i|Paste failed|]})
  -- index' sessionId


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
