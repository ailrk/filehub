{-# LANGUAGE PartialTypeSignatures #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Utilities for Server.
module Filehub.Server.Internal
  ( makeTemplateContext
  , withQueryParam
  , clear
  , parseHeader'
  )
  where

import Data.ByteString (ByteString)
import Effectful ( Eff, (:>), IOE )
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.Reader.Dynamic (Reader, asks, ask)
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import Filehub.Env qualified as Env
import Filehub.Error (withServerError)
import Filehub.Monad (Filehub)
import Filehub.Session qualified as Session
import Filehub.Template.Internal (TemplateContext(..))
import Filehub.Types
import Lens.Micro ((&))
import Lens.Micro.Platform ()
import Prelude hiding (elem)
import Prelude hiding (readFile)
import Servant ( ServerError(..), ServerError, FromHttpApiData (..) )
import Servant.Server (err400)


makeTemplateContext :: SessionId -> Filehub TemplateContext
makeTemplateContext sessionId = do
  theme             <- Session.getSessionTheme sessionId      & withServerError
  layout            <- Session.getLayout sessionId            & withServerError
  readOnly          <- asks @Env (.readOnly)
  noLogin           <- Env.hasNoLogin <$> ask @Env
  display           <- Session.getDisplay sessionId           & withServerError
  state             <- Session.getControlPanelState sessionId & withServerError
  root              <- Session.getRoot sessionId              & withServerError
  sortedBy          <- Session.getSortFileBy sessionId        & withServerError
  selected          <- Selected.getSelected sessionId         & withServerError
  currentDir        <- Session.getCurrentDir sessionId        & withServerError
  currentTarget     <- Session.currentTarget sessionId        & withServerError
  locale            <- Session.getSessionLocale sessionId     & withServerError
  simpleAuthUserDB  <- asks @Env (.simpleAuthUserDB)
  oidcAuthProviders <- asks @Env (.oidcAuthProviders)
  pure TemplateContext
    { readOnly           = readOnly
    , noLogin            = noLogin
    , display            = display
    , layout             = layout
    , theme              = theme
    , sortedBy           = sortedBy
    , selected           = selected
    , state              = state
    , root               = root
    , locale             = locale
    , currentDir         = currentDir
    , currentTarget      = currentTarget
    , simpleAuthUserDB   = simpleAuthUserDB
    , oidcAuthProviders  = oidcAuthProviders
    }


-- | Ensure a query parameter presents, otherwise it's a client error
withQueryParam :: (Error ServerError :> es) => Maybe a -> Eff es a
withQueryParam m =
  case m of
    Just a  -> pure a
    Nothing -> throwError err400


-- | Completely reset all state machines. This should be the only place to reset state.
clear :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clear sessionId = do
  Selected.clearSelectedAllTargets sessionId
  Copy.clearCopyState sessionId


parseHeader' :: FromHttpApiData a => ByteString -> Maybe a
parseHeader' x = either (const Nothing) Just (parseHeader x)
