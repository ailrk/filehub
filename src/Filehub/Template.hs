{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template
  ( TemplateContext(..)
  , Template
  , runTemplate
  , makeTemplateContext
  )
  where

import Control.Monad.Reader (ReaderT, runReader, asks, MonadReader (..))
import Data.ClientPath (AbsPath, Root)
import Data.Functor.Identity (Identity)
import Filehub.Auth.Types.OIDC (OIDCAuthProviders)
import Filehub.Auth.Types.Simple (SimpleAuthUserDB)
import Filehub.Env qualified as Env
import Filehub.Locale (Locale)
import Filehub.Monad (Filehub)
import Filehub.Session (Session(..), getRoot, getCurrentTarget, getTargetViews)
import Filehub.Session (TargetView(..), SessionId)
import Filehub.Session.Types (Selected, Layout, ControlPanelState)
import Filehub.Sort (SortFileBy)
import Filehub.Theme (Theme)
import Filehub.Types (Display(..), Env)
import Filehub.Session.Pool (withSession)
import Filehub.Session.Handle (getDisplay, getControlPanelState)
import Filehub.Session.Types (TargetSessionData(..))
import Filehub.Session.Selected (getAllSelected)


-- | A Template context type that capture all useful information to render
-- a HTML.
--
-- == On laziness
--   All fields are lazy. Most of these fields are from a session which requires a
--   Map lookup with a sessionId, if the record is strict, we need to perform lookup for
--   every field even when we don't need them. Being lazy means we pay exactly what we
--   need, which is pretty good.
data TemplateContext = TemplateContext
  { readOnly          :: ~Bool
  , noLogin           :: ~Bool
  , display           :: ~Display
  , sidebarCollapsed  :: ~Bool
  , layout            :: ~Layout
  , theme             :: ~Theme
  , selected          :: ~Selected
  , sortedBy          :: ~SortFileBy
  , locale            :: ~Locale
  , state             :: ~ControlPanelState
  , currentDir        :: ~AbsPath
  , currentTarget     :: ~TargetView
  , root              :: ~Root
  , simpleAuthUserDB  :: ~SimpleAuthUserDB
  , oidcAuthProviders :: ~OIDCAuthProviders
  }


runTemplate :: TemplateContext -> Template a -> a
runTemplate ctx = flip runReader ctx


type Template = ReaderT TemplateContext Identity


makeTemplateContext :: SessionId -> Filehub TemplateContext
makeTemplateContext sessionId = do
  env <- ask
  readOnly          <- asks (.readOnly)
  noLogin           <- Env.hasNoLogin <$> ask @Env
  simpleAuthUserDB  <- asks (.simpleAuthUserDB)
  oidcAuthProviders <- asks (.oidcAuthProviders)

  withSession sessionId \s -> do
    display       <- getDisplay s
    root          <- getRoot env s
    currentTarget <- getCurrentTarget env s
    targetViews   <- getTargetViews env s
    state         <- getControlPanelState (getAllSelected targetViews) s
    pure TemplateContext
      { readOnly           = readOnly
      , noLogin            = noLogin
      , display            = display
      , sidebarCollapsed   = s.sidebarCollapsed
      , layout             = s.layout
      , theme              = s.theme
      , sortedBy           = currentTarget.sessionData.sortedFileBy
      , selected           = currentTarget.sessionData.selected
      , state              = state
      , root               = root
      , locale             = s.locale
      , currentDir         = currentTarget.sessionData.currentDir
      , currentTarget      = currentTarget
      , simpleAuthUserDB   = simpleAuthUserDB
      , oidcAuthProviders  = oidcAuthProviders
      }
