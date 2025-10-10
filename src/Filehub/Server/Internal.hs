{-# LANGUAGE PartialTypeSignatures #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Utilities for Server.
module Filehub.Server.Internal
  ( withQueryParam
  , clear
  , parseHeader'
  )
  where

import Data.ByteString (ByteString)
import Effectful.Error.Dynamic (throwError)
import Filehub.Error (FilehubError (..))
import Filehub.Monad (Filehub)
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import Filehub.Types
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude hiding (readFile)
import Servant ( FromHttpApiData (..) )
import Servant.Server (err400)


-- | Ensure a query parameter presents, otherwise it's a client error
withQueryParam :: Maybe a -> Filehub a
withQueryParam m =
  case m of
    Just a  -> pure a
    Nothing -> throwError do HTTPError err400


-- | Completely reset all state machines. This should be the only place to reset state.
clear :: SessionId -> Filehub ()
clear sessionId = do
  Selected.clearSelectedAllTargets sessionId
  Copy.clearCopyState sessionId


parseHeader' :: FromHttpApiData a => ByteString -> Maybe a
parseHeader' x = either (const Nothing) Just (parseHeader x)
