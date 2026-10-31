{-# LANGUAGE MultiParamTypeClasses #-}
module Filehub.Routes.LinkConf
  ( apiLinksConf
  , LinkConf(..)
  , AsLinkConf
  )
  where

import Filehub.Routes (Api(..))
import Servant
import Data.Text (Text)
import Lucid.Htmx (Swap (..))
import GHC.Generics
import Filehub.Types (FilehubEvent (..))


data LinkConf = LinkConf
  { hxTarget :: Maybe Text
  , hxSwap   :: Swap
  , hxOn     :: Maybe (FilehubEvent, Text)
  }


data AsLinkConf


instance GenericMode AsLinkConf where
  type AsLinkConf :- api = HFromApi api


type family HFromApi api where
  HFromApi ("cd" :> r)                = LinkConf
  HFromApi ("files" :> r)             = LinkConf
  HFromApi ("modal" :> r)             = LinkConf
  HFromApi ("folders" :> r)           = LinkConf
  HFromApi ("search" :> r)            = LinkConf
  HFromApi ("table" :> r)             = LinkConf
  HFromApi ("layout" :> r)            = LinkConf
  HFromApi ("upload" :> r)            = LinkConf
  HFromApi ("cancel" :> r)            = LinkConf
  HFromApi ("logout" :> r)            = LinkConf
  HFromApi ("theme" :> r)             = LinkConf
  HFromApi ("target" :> r)            = LinkConf
  HFromApi ("sidebar" :> r)           = LinkConf
  HFromApi ("login" :> "theme" :> r)  = LinkConf
  HFromApi ("login" :> "locale" :> r) = LinkConf
  HFromApi ("locale" :> r)            = LinkConf
  HFromApi x                          = ()


apiLinksConf :: Api AsLinkConf
apiLinksConf = emptyApiConf
  { cd                = LinkConf { hxTarget = Nothing
                                 , hxSwap   = None
                                 , hxOn     = Just (DirChanged, "window.handleChangeDir(event)")
                                 }
  , newFile           = LinkConf Nothing None Nothing
  , newFolder         = LinkConf Nothing None Nothing
  , updateFile        = LinkConf Nothing None Nothing
  , search            = LinkConf (Just "#table") OuterHTML Nothing
  , editorModal       = LinkConf (Just "#index") BeforeEnd Nothing
  , newFolderModal    = LinkConf (Just "#index") BeforeEnd Nothing
  , newFileModal      = LinkConf (Just "#index") BeforeEnd Nothing
  , changeTarget      = LinkConf { hxTarget = Just "#index"
                                 , hxSwap   = OuterHTML
                                 , hxOn     = Just (TargetChanged, "window.handleChangeTarget(even)")
                                 }
  , upload            = LinkConf (Just "#index") OuterHTML Nothing
  , copy              = LinkConf (Just "#control-panel") OuterHTML Nothing
  , paste             = LinkConf Nothing None Nothing
  , delete            = LinkConf Nothing None Nothing
  , cancel            = LinkConf Nothing None Nothing
  , toggleSidebar     = LinkConf { hxTarget = Nothing
                                 , hxSwap   = None
                                 , hxOn     = Just (SidebarToggled, "window.handleToggleSidebar(event)")
                                 }
  , toggleTheme       = LinkConf { hxTarget = (Just "#index")
                                 , hxSwap   = OuterHTML
                                 , hxOn     = Just (ThemeChanged, "window.handleChangeTheme(event)")
                                 }
  , selectLayout      = LinkConf { hxTarget = Just "#index"
                                 , hxSwap   = OuterHTML
                                 , hxOn     = Just (LayoutChanged, "window.handleChangeLayout(event)")
                                 }
  , changeLocale      = LinkConf { hxTarget = Just "#index"
                                 , hxSwap   = OuterHTML
                                 , hxOn     = Just (LocaleChanged, "window.handleChangeLocale(event)")
                                 }
  , rename            = LinkConf { hxTarget = Just "#view"
                                 , hxSwap   = OuterHTML
                                 , hxOn     = Just (FileRenamed, "window.handleRenameFile(event)")
                                 }
  , sortTable         = LinkConf { hxTarget = Just "#index"
                                 , hxSwap   = OuterHTML
                                 , hxOn     = Just (TableSorted, "window.handleSortTable(event)")
                                 }
  , copy1             = LinkConf (Just "#index") OuterHTML Nothing
  , renameModal       = LinkConf (Just "#index") BeforeEnd Nothing
  , fileDetailModal   = LinkConf (Just "#index") BeforeEnd Nothing
  , loginChangeLocale = LinkConf { hxTarget = Just "#login"
                                 , hxSwap   = OuterHTML
                                 , hxOn     = Just (LocaleChanged, "window.handleChangeLocale(event)")
                                 }
  , loginToggleTheme  = LinkConf { hxTarget = Just "#login"
                                 , hxSwap   = OuterHTML
                                 , hxOn     = Just (ThemeChanged, "window.handleChangeTheme(event)")
                                 }
  , move              = LinkConf { hxTarget = Nothing
                                 , hxSwap   = None
                                 , hxOn     = Just (FileMoved, "window.handleMoveFile(event)")
                                 }
  }


emptyApiConf :: Api AsLinkConf
emptyApiConf = to gdefaults


-- | A class that builds an Api record by defaulting everything to () or a
-- baseline LinkConf
class GDefaults f where
  gdefaults :: f p


instance GDefaults f => GDefaults (M1 i c f) where
  gdefaults = M1 gdefaults


instance (GDefaults a, GDefaults b) => GDefaults (a :*: b) where
  gdefaults = gdefaults :*: gdefaults


instance GDefaults (K1 i ()) where
  gdefaults = K1 ()


instance GDefaults (K1 i LinkConf) where
  gdefaults = K1 LinkConf { hxTarget = Nothing
                          , hxSwap   = None
                          , hxOn     = Nothing
                          }
