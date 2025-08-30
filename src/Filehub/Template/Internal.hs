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


data TemplateContext = TemplateContext
  { readOnly      :: Bool
  , noLogin       :: Bool
  , display       :: Display
  , layout        :: Layout
  , theme         :: Theme
  , selected      :: Selected
  , sortedBy      :: SortFileBy
  , state         :: ControlPanelState
  , currentDir    :: FilePath
  , currentTarget :: TargetView
  , root          :: FilePath
  }


runTemplate :: TemplateContext -> Template a -> a
runTemplate ctx = runPureEff . runReader ctx


type Template =  Eff '[Reader TemplateContext]
