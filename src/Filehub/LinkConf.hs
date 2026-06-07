{-# LANGUAGE MultiParamTypeClasses #-}
module Filehub.LinkConf
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


data LinkConf = LinkConf
  { target         :: Maybe Text
  , swap           :: Swap
  }


data AsLinkConf


instance GenericMode AsLinkConf where
  type AsLinkConf :- api = HFromApi api


type family HFromApi api where
  HFromApi ("cd" :> r)                            = LinkConf
  HFromApi ("files" :> "new" :> r)                = LinkConf
  HFromApi ("files" :> "update" :> r)             = LinkConf
  HFromApi ("files" :> "rename" :> r)             = LinkConf
  HFromApi ("files" :> "delete" :> r)             = LinkConf
  HFromApi ("files" :> "copy" :> r)               = LinkConf
  HFromApi ("files" :> "copy1" :> r)              = LinkConf
  HFromApi ("files" :> "paste" :> r)              = LinkConf
  HFromApi ("files" :> "move" :> r)               = LinkConf
  HFromApi ("modal" :> "rename" :> r)             = LinkConf
  HFromApi ("folders" :> "new" :> r)              = LinkConf
  HFromApi ("modal" :> "new-file" :> r)           = LinkConf
  HFromApi ("modal" :> "new-folder" :> r)         = LinkConf
  HFromApi ("modal" :> "file" :> "detail" :> r)   = LinkConf
  HFromApi ("modal" :> "editor" :> r)             = LinkConf
  HFromApi ("search" :> r)                        = LinkConf
  HFromApi ("table" :> "sort" :> r)               = LinkConf
  HFromApi ("layout" :> r)                        = LinkConf
  HFromApi ("table" :> "select" :> r)             = LinkConf
  HFromApi ("upload" :> r)                        = LinkConf
  HFromApi ("cancel" :> r)                        = LinkConf
  HFromApi ("target" :> "change" :> r)            = LinkConf
  HFromApi ("sidebar" :> "toggle" :> r)           = LinkConf
  HFromApi ("theme" :> "toggle" :> r)             = LinkConf
  HFromApi ("locale" :> "change" :> r)            = LinkConf
  HFromApi ("login" :> "theme" :> "toggle" :> r)  = LinkConf
  HFromApi ("login" :> "locale" :> "change" :> r) = LinkConf
  HFromApi ("logout" :> r)                        = LinkConf
  HFromApi x                                      = ()


apiLinksConf :: Api AsLinkConf
apiLinksConf = emptyApiConf
  { cd                = LinkConf Nothing None
  , newFile           = LinkConf Nothing None
  , newFolder         = LinkConf Nothing None
  , updateFile        = LinkConf Nothing None
  , search            = LinkConf (Just "#table") OuterHTML
  , editorModal       = LinkConf (Just "#index") BeforeEnd
  , changeTarget      = LinkConf (Just "#index") OuterHTML
  , newFolderModal    = LinkConf (Just "#index") BeforeEnd
  , newFileModal      = LinkConf (Just "#index") BeforeEnd
  , upload            = LinkConf (Just "#index") OuterHTML
  , copy              = LinkConf (Just "#control-panel") OuterHTML
  , paste             = LinkConf Nothing None
  , delete            = LinkConf Nothing None
  , cancel            = LinkConf Nothing None
  , toggleSidebar     = LinkConf (Just "#index") OuterHTML
  , toggleTheme       = LinkConf (Just "#index") OuterHTML
  , selectLayout      = LinkConf (Just "#index") OuterHTML
  , changeLocale      = LinkConf (Just "#index") OuterHTML
  , rename            = LinkConf (Just "#view") OuterHTML
  , sortTable         = LinkConf (Just "#index") OuterHTML
  , copy1             = LinkConf (Just "#index") OuterHTML
  , renameModal       = LinkConf (Just "#index") BeforeEnd
  , fileDetailModal   = LinkConf (Just "#index") BeforeEnd
  , loginChangeLocale = LinkConf (Just "#login") OuterHTML
  , loginToggleTheme  = LinkConf (Just "#login") OuterHTML
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
  gdefaults = K1 (LinkConf Nothing None)
