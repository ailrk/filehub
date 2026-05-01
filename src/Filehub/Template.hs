{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template
  ( TemplateContext(..)
  , Template
  , runTemplate
  , makeTemplateContext
  )
  where

import Filehub.Locale (Locale)
import Filehub.Sort (SortFileBy)
import Filehub.Types (Display(..), Env)
import Lens.Micro.Platform ()
import Filehub.Session (TargetView(..), SessionId)
import Filehub.Theme (Theme)
import Filehub.Auth.Simple (SimpleAuthUserDB)
import Filehub.Auth.OIDC (OIDCAuthProviders)
import Filehub.Env qualified as Env
import Data.ClientPath (AbsPath, Root)
import Filehub.Session (SessionGet(..))
import Filehub.Session qualified as Session
import Data.Functor.Identity (Identity)
import Control.Monad.Reader (ReaderT, runReader, asks, MonadReader (..))
import Filehub.Monad (Filehub)
import Filehub.Session.Types (Selected, Layout, ControlPanelState)


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
  display           <- Session.get sessionId (.display)
  sidebarCollapsed  <- Session.get sessionId (.sidebarCollapsed)
  layout            <- Session.get sessionId (.layout)
  theme             <- Session.get sessionId (.theme)
  sortedBy          <- Session.get sessionId (.sortedFileBy)
  state             <- Session.get sessionId (.controlPanelState)
  selected          <- Session.get sessionId (.selected)
  root              <- Session.get sessionId (.root)
  locale            <- Session.get sessionId (.locale)
  currentDir        <- Session.get sessionId (.currentDir)
  currentTarget     <- Session.get sessionId (.currentTarget)
  readOnly          <- asks (.readOnly)
  noLogin           <- Env.hasNoLogin <$> ask @Env
  simpleAuthUserDB  <- asks (.simpleAuthUserDB)
  oidcAuthProviders <- asks (.oidcAuthProviders)
  pure TemplateContext
    { readOnly           = readOnly
    , noLogin            = noLogin
    , display            = display
    , sidebarCollapsed   = sidebarCollapsed
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
