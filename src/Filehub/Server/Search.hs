module Filehub.Server.Search where

import Filehub.Handler (ConfirmLogin)
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Session qualified as Session
import Filehub.Session (SessionId(..), getDisplay, makeStorageDyn)
import Filehub.Template (runTemplate, makeTemplateContext)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Template.Shared qualified as Template
import Filehub.Types (Display (..), SearchWord)
import Lucid hiding (for_)
import Prelude hiding (init, readFile)
import Filehub.Session.Pool (withSession)
import Control.Monad (join)


search :: SessionId -> ConfirmLogin -> SearchWord -> Filehub (Html ())
search sessionId _ searchWord = do
  join $ withSession sessionId \s -> do
    display <- getDisplay s
    pure do
      ctx     <- makeTemplateContext sessionId
      storage <- makeStorageDyn sessionId
      files   <- storage.lsCwd
      case display of
        Mobile    -> pure $ runTemplate ctx (Template.search searchWord files Template.Mobile.table)
        Desktop   -> pure $ runTemplate ctx (Template.search searchWord files Template.Desktop.table)
        NoDisplay -> error "impossible"
