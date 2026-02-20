{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template
  ( TemplateContext(..)
  , Template
  , runTemplate
  , makeTemplateContext
  )
  where

import Effectful.Reader.Dynamic ( asks, ask, Reader, runReader )
import Filehub.Locale ( Locale )
import Filehub.Sort ( SortFileBy )
import Filehub.Types
    ( Display(..),
      ControlPanelState(..),
      Env,
      Layout,
      Selected )
import Lens.Micro.Platform ()
import Filehub.Session (TargetView(..), SessionId)
import Filehub.Theme (Theme)
import Effectful (Eff, runPureEff)
import Filehub.Auth.Simple (SimpleAuthUserDB)
import Filehub.Auth.OIDC (OIDCAuthProviders)
import Filehub.Monad (IsFilehub)
import Filehub.Env qualified as Env
import Data.ClientPath (AbsPath)
import Filehub.Session.Effectful (runSessionEff, SessionGet(..))
import Filehub.Session.Effectful qualified as Session


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
  , root              :: ~AbsPath
  , simpleAuthUserDB  :: ~SimpleAuthUserDB
  , oidcAuthProviders :: ~OIDCAuthProviders
  }


runTemplate :: TemplateContext -> Template a -> a
runTemplate ctx = runPureEff . runReader ctx


type Template =  Eff '[Reader TemplateContext]


makeTemplateContext :: IsFilehub es => SessionId -> Eff es TemplateContext
makeTemplateContext sessionId = runSessionEff sessionId do
  display           <- Session.get (.display)
  sidebarCollapsed  <- Session.get (.sidebarCollapsed)
  layout            <- Session.get (.layout)
  theme             <- Session.get (.theme)
  sortedBy          <- Session.get (.sortedFileBy)
  state             <- Session.get (.controlPanelState)
  selected          <- Session.get (.selected)
  root              <- Session.get (.root)
  locale            <- Session.get (.locale)
  currentDir        <- Session.get (.currentDir)
  currentTarget     <- Session.get (.currentTarget)
  readOnly          <- asks @Env (.readOnly)
  noLogin           <- Env.hasNoLogin <$> ask @Env
  simpleAuthUserDB  <- asks @Env (.simpleAuthUserDB)
  oidcAuthProviders <- asks @Env (.oidcAuthProviders)
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
