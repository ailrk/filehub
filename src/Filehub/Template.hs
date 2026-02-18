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
import Filehub.Session qualified as Session
import Filehub.Session.Selected qualified as Session
import Filehub.Theme (Theme)
import Effectful (Eff, runPureEff)
import Filehub.Auth.Simple (SimpleAuthUserDB)
import Filehub.Auth.OIDC (OIDCAuthProviders)
import Filehub.Monad (Filehub)
import Filehub.Env qualified as Env
import Data.ClientPath (AbsPath)


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


makeTemplateContext :: SessionId -> Filehub TemplateContext
makeTemplateContext sessionId = do
  theme             <- Session.getSessionTheme sessionId
  sidebarCollapsed  <- Session.getSidebarCollapsed sessionId
  layout            <- Session.getLayout sessionId
  readOnly          <- asks @Env (.readOnly)
  noLogin           <- Env.hasNoLogin <$> ask @Env
  display           <- Session.getDisplay sessionId
  state             <- Session.getControlPanelState sessionId
  root              <- Session.getRoot sessionId
  sortedBy          <- Session.getSortFileBy sessionId
  selected          <- Session.getSelected sessionId
  currentDir        <- Session.getCurrentDir sessionId
  currentTarget     <- Session.currentTarget sessionId
  locale            <- Session.getSessionLocale sessionId
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

