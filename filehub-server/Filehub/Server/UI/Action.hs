{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Utilities for Server.
module Filehub.Server.UI.Action
  ( clear
  , selectLayout
  , sortTable
  , toggleTheme
  , changeLocale
  , toggleSidebar
  , selectRows
  , cancel
  )
  where

import Filehub.Prelude hiding (cancel)
import Control.Monad.Reader (MonadReader(ask))
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Handler (ConfirmLogin)
import Filehub.Locale (Locale)
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Orphan ()
import Filehub.Server.UI.Render (index, UIPartialUpdate (..), runUIPartialUpdate)
import Filehub.Session (TargetView (..), getTargetViews, getCurrentTarget, withSession, withSession_, modifySession, AllSelected(..), Selected(..), Layout(..))
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import Filehub.Theme qualified as Theme
import Filehub.Types
import Lucid hiding ()
import Prelude hiding (elem, readFile, init)
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader, NoContent (..))


-- | Reset navigation-sensitive state: selections and UI.
-- Does NOT clear copy state — that persists across target switches so paste
-- remains available after the user navigates to the destination target.
clear :: SessionId -> Filehub ()
clear sessionId =
  Selected.clearSelectedAllTargets sessionId


selectLayout :: SessionId -> ConfirmLogin -> Maybe Layout -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
selectLayout sessionId _ layout = do
  modifySession sessionId \s -> pure do
    s { layout = (fromMaybe ThumbnailLayout layout)
      }
  addHeader LayoutChanged <$> index sessionId


sortTable :: SessionId -> ConfirmLogin -> Maybe SortFileBy -> Filehub NoContent
sortTable sessionId _ order = do
  env <- ask
  withSession sessionId \s -> do
    TargetView _ td <- getCurrentTarget env s
    writeTVar td.sortedFileBy (fromMaybe ByNameUp order)
  pure NoContent


-- | Toggle the frontend theme by triggering the event handler of `ThemeChanged`
-- in the frontend. A fade-in animation is played when the theme toggled, and it needs
-- to be removed by the frontend.
toggleTheme :: SessionId -> ConfirmLogin -> Filehub NoContent
toggleTheme sessionId _ = do
  withSession_ sessionId \s -> pure
    case s.theme of
      Theme.Light -> ( s { theme = Dark }, ())
      Theme.Dark  -> ( s { theme = Light }, ())
  pure NoContent


changeLocale :: SessionId -> Maybe Locale -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
changeLocale _ Nothing = throwIO (FilehubError LocaleError "Invalid locale")
changeLocale sessionId (Just locale) = do
  withSession_ sessionId \s -> pure (s { locale = locale } , ())
  addHeader LocaleChanged <$> index sessionId


toggleSidebar :: SessionId -> ConfirmLogin -> Filehub NoContent
toggleSidebar sessionId _ = do
  withSession_ sessionId \s -> pure
    ( s { sidebarCollapsed = not s.sidebarCollapsed
        }
    , ()
    )
  pure NoContent


selectRows :: SessionId -> ConfirmLogin -> Selected -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
selectRows sessionId _ selected = do
  env <- ask
  case selected of
    NoSelection -> do
      withSession sessionId \s -> do
        TargetView _ td <- getCurrentTarget env s
        writeTVar td.selected NoSelection

    _ -> do
      withSession sessionId \s -> do
        TargetView _ td <- getCurrentTarget env s
        writeTVar td.selected selected

  AllSelected { count } <- withSession sessionId \s -> do
    Selected.getAllSelected =<< getTargetViews env s

  addHeader count <$> runUIPartialUpdate sessionId
    [ UpdateSideBar
    , UpdateControlPanel
    ]


cancel :: SessionId -> ConfirmLogin -> Filehub (Headers '[Header "X-Filehub-Selected-Count" Int] (Html ()))
cancel sessionId _ = do
  env <- ask

  count <- withSession sessionId \s -> do
    AllSelected { count } <- Selected.getAllSelected =<< getTargetViews env s
    pure count

  Copy.clearCopyState sessionId
  clear sessionId
  addHeader count <$> runUIPartialUpdate sessionId
    [ UpdateControlPanel
    , UpdateSideBar
    , UpdateView
    ]
