{-# LANGUAGE PartialTypeSignatures #-}
module Filehub.Server.Internal where

import Effectful ( Eff, (:>), IOE )
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Env (Env (..))
import Filehub.Error ( withServerError, FilehubError(..), FilehubError(..), withServerError )
import Filehub.Selected qualified as Selected
import Filehub.Storage (Storage)
import Filehub.Storage qualified as Storage
import Filehub.Types
    ( SessionId(..),
      SessionId(..))
import Filehub.Copy qualified as Copy
import Lens.Micro.Platform ()
import Prelude hiding (readFile)
import Prelude hiding (readFile)
import Servant ( ServerError(..), ServerError )
import Servant.Server (err400)


withQueryParam :: (Error ServerError :> es) => Maybe a -> Eff es a
withQueryParam m =
  case m of
    Just a -> pure a
    Nothing -> throwError err400


runStorage :: _ => SessionId -> Eff (Storage : Error FilehubError : es) a -> Eff es a
runStorage sessionId = withServerError . Storage.runStorage sessionId


clear :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clear sessionId = do
  Selected.clearSelectedAllTargets sessionId
  Copy.clearCopyState sessionId
