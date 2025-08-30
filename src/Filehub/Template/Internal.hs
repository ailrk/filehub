module Filehub.Template.Internal where


import Filehub.Types ( Display(..), ControlPanelState (..))
import Lens.Micro.Platform ()
import Filehub.Layout (Layout)
import Filehub.Theme (Theme)
import Effectful.Reader.Dynamic (Reader, runReader)
import Effectful (Eff, runPureEff)
import Filehub.Selected.Types (Selected)
import Filehub.Sort (SortFileBy)
import Filehub.Session (TargetView)
import Filehub.Locale (Locale)
import Filehub.Auth.Simple (SimpleAuthUserDB)
import Filehub.Auth.OIDC (OIDCAuthProviders)


-- | A Template context type that capture all useful information to render
-- a HTML.
--
-- == On laziness
--   All fields are lazy. Most of these fields are from a session which requires a
--   Map lookup with a sessionId, if the record is strict, we need to perform lookup for
--   every field even when we don't need them. Being lazy means we pay exactly what we
--   need, which is pretty good.
data TemplateContext = TemplateContext
  { readOnly             :: ~Bool
  , noLogin              :: ~Bool
  , display              :: ~Display
  , layout               :: ~Layout
  , theme                :: ~Theme
  , selected             :: ~Selected
  , sortedBy             :: ~SortFileBy
  , locale               :: ~Locale
  , state                :: ~ControlPanelState
  , currentDir           :: ~FilePath
  , currentTarget        :: ~TargetView
  , root                 :: ~FilePath
  , simpleAuthUserDB :: ~SimpleAuthUserDB
  , oidcAuthProviders    :: ~OIDCAuthProviders
  }


runTemplate :: TemplateContext -> Template a -> a
runTemplate ctx = runPureEff . runReader ctx


type Template =  Eff '[Reader TemplateContext]
