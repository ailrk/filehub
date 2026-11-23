module Filehub.Plugin.Types
  ( StaticPlugin(..)
  , ContextMenuEntry(..)
  , ViewerEntry(..)
  , ToolbarEntry(..)
  , UrlTemplate(..)
  , EntryAction(..)
  , StaticPlugins(..)
  ) where

import Data.Text (Text)


newtype UrlTemplate = UrlTemplate { unUrlTemplate :: Text }
  deriving (Show, Eq)

data EntryAction
  = UrlAction  UrlTemplate
  | ScriptAction Text
  deriving (Show, Eq)

data ContextMenuEntry = ContextMenuEntry
  { mime   :: Text
  , label  :: Text
  , action :: EntryAction
  } deriving (Show, Eq)

data ViewerEntry = ViewerEntry
  { mime   :: Text
  , action :: EntryAction
  } deriving (Show, Eq)

data ToolbarEntry = ToolbarEntry
  { mime   :: Text
  , label  :: Text
  , action :: EntryAction
  } deriving (Show, Eq)

data StaticPlugin = StaticPlugin
  { name        :: Text
  , contextMenu :: [ContextMenuEntry]
  , viewer      :: [ViewerEntry]
  , toolbar     :: [ToolbarEntry]
  } deriving (Show, Eq)

newtype StaticPlugins = StaticPlugins { unStaticPlugins :: [StaticPlugin] }
  deriving newtype (Semigroup, Monoid)
  deriving (Show, Eq)
