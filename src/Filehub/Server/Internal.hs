{-# LANGUAGE PartialTypeSignatures #-}
module Filehub.Server.Internal where

import Effectful ( Eff, (:>), IOE, MonadUnliftIO (..) )
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.Reader.Dynamic (Reader, asks, ask)
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
import Filehub.Template.Internal (TemplateContext(..))
import Filehub.Error (withServerError)
import Control.Exception (SomeException, catch)
import Data.String.Interpolate (i)
import Lens.Micro ((&))
import Effectful.Log (logAttention)
import Filehub.Session qualified as Session
import Filehub.Env qualified as Env
import Filehub.ControlPanel qualified as ControlPanel


makeTemplateContext :: SessionId -> Filehub TemplateContext
makeTemplateContext sessionId = do
  theme <- Session.getSessionTheme sessionId & withServerError
  layout <- Session.getLayout sessionId & withServerError
  readOnly <- asks @Env (.readOnly)
  noLogin <- Env.hasNoLogin <$> ask @Env
  display <- Session.getDisplay sessionId & withServerError
  state <- ControlPanel.getControlPanelState sessionId & withServerError
  root <- Session.getRoot sessionId & withServerError
  sortedBy <- Session.getSortFileBy sessionId & withServerError
  selected <- Selected.getSelected sessionId & withServerError
  currentDir <- Session.getCurrentDir sessionId & withServerError
  currentTarget <- Session.currentTarget sessionId & withServerError
  pure TemplateContext
    { readOnly      = readOnly
    , noLogin       = noLogin
    , display       = display
    , layout        = layout
    , theme         = theme
    , sortedBy      = sortedBy
    , selected      = selected
    , state         = state
    , root          = root
    , currentDir    = currentDir
    , currentTarget = currentTarget
    }


copy :: SessionId -> Filehub ()
copy sessionId = withServerError do
  Copy.select sessionId
  Copy.copy sessionId


paste :: SessionId -> Filehub ()
paste sessionId = do
  withRunInIO $ \unlift -> do
    unlift (Copy.paste sessionId & withServerError) `catch` \(e :: SomeException) -> unlift do
      logAttention [i|Paste Failed |] (show e)
      throwError (err500 { errBody = [i|Paste failed #{e}|]})


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
