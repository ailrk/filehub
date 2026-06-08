{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Lucid.Htmx where

import Data.Char (isUpper, toLower)
import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttribute)
import Servant (Link)
import Data.Text qualified as T
import Servant.Extended (linkToText)


data Swap
    = InnerHTML
    | OuterHTML
    | BeforeBegin
    | AfterBegin
    | BeforeEnd
    | AfterEnd
    | Delete
    | None
    deriving (Eq)


instance Show Swap where
    show InnerHTML   = "innerHTML"
    show OuterHTML   = "outerHTML"
    show BeforeBegin = "beforebegin"
    show AfterBegin  = "afterbegin"
    show BeforeEnd   = "beforeend"
    show AfterEnd    = "afterend"
    show Delete      = "delete"
    show None        = "none"


data Trigger
    = Click
    | Change
    | Submit
    | Load
    | Intersect
    | Every Text -- e.g., Every "5s"
    deriving (Eq)


instance Show Trigger where
    show Click     = "click"
    show Change    = "change"
    show Submit    = "submit"
    show Load      = "load"
    show Intersect = "intersect"
    show (Every t) = "every " <> T.unpack t


-- | Htmx event.
-- The case is tricky. The htmx conventions is that the event itself is in
-- camel case, e.g `htmx:BeforeRequest`, but when it's referrered in html, it is
-- converted into kebab case, e.g `htmx:before-request`.
--
-- `htmxEventAsCamel` and `htmxEventAsKebab` are two only ways to convert a
-- `HtmxEvent` to `Text.
data HtmxEvent
    = BeforeRequest
    | BeforeSend
    | AfterOnLoad
    | AfterRequest
    | AfterSettle
    | AfterSwap
    | OobBeforeSwap
    | OobAfterSwap
    | ResponseError
    | SendError
    deriving (Eq, Show)


-- Custom events should have the same naming convention as the standard htmx
-- evenst. If we have an event `ThemeToggled`, the event should be referred as
-- `themeToggled` in js, and `theme-toggled` in htmx.
newtype HtmxCustomEvent a = HtmxCustomEvent a


class IsHtmxCustomEvent a where
  -- | Convert a custom event to a text that's suitable for htmx.
  -- The text should be Pascal Case, which is the default Show instance for
  -- most sum types. These two functions will transform it to the proper
  -- casing.
  htmxCustomEventNameToText :: a -> Text


asCamelCase :: Text -> Text
asCamelCase t =
  case T.uncons t of
    Just (c, cs) -> T.cons (toLower c) cs
    Nothing -> t


htmxEventAsCamel :: HtmxEvent -> Text
htmxEventAsCamel e = asCamelCase (T.pack (show e))


htmxEventAsKebab :: HtmxEvent -> Text
htmxEventAsKebab e = toLCKebab (htmxEventAsCamel e)


-- | Convert camel case string to lower case kebab case.
toLCKebab :: Text -> Text
toLCKebab t =
  case T.uncons t of
    Just (c, cs) ->  T.cons (toLower c) (T.foldr trans T.empty cs)
    Nothing      -> T.empty
  where
    trans c acc
      | isUpper c = T.cons '-' (T.cons (toLower c) acc)
      | otherwise = T.cons c acc


-- | Method Attributes (taking Link)
hxGet :: Link -> Attribute
hxGet = makeAttribute "hx-get" . linkToText


class HxPost a where
  hxPost :: a -> Attribute


instance HxPost Link where
  hxPost = makeAttribute "hx-post" . linkToText


instance HxPost Text where
  hxPost = makeAttribute "hx-post"


hxDelete :: Link -> Attribute
hxDelete = makeAttribute "hx-delete" . linkToText


hxConfirm :: Text -> Attribute
hxConfirm = makeAttribute "hx-confirm"


hxEncoding :: Text -> Attribute
hxEncoding = makeAttribute "hx-encoding"


hxTarget :: Text ->  Attribute
hxTarget = makeAttribute "hx-target"


------------------------------
-- On

class HxOn a where
  hxOn :: a -> Text -> Attribute


instance HxOn Text where
  hxOn event script = makeAttribute ("hx-on:" <> event) script


instance HxOn HtmxEvent where
  hxOn event script =
    let eventName = htmxEventAsKebab event
        cleanName = T.replace "htmx:" "" eventName
    in makeAttribute ("hx-on::" <> cleanName) script


instance IsHtmxCustomEvent evt => HxOn (HtmxCustomEvent evt) where
  hxOn (HtmxCustomEvent event) script =
    makeAttribute ("hx-on:" <> toLCKebab (htmxCustomEventNameToText event)) script


instance HxOn Trigger where
  hxOn event script =
    let
        eventName = T.pack (show event)
        cleanName = T.replace "htmx:" "" eventName
    in
        makeAttribute ("hx-on::" <> cleanName) script


------------------------------
-- Trigger

class HxTrigger a where
    hxTrigger :: a -> Attribute


instance HxTrigger Trigger where
    hxTrigger t = makeAttribute "hx-trigger" (T.pack $ show t)


instance HxTrigger HtmxEvent where
    hxTrigger e = makeAttribute "hx-trigger" (htmxEventAsKebab e)


instance IsHtmxCustomEvent evt => HxTrigger (HtmxCustomEvent evt) where
    hxTrigger (HtmxCustomEvent e) = makeAttribute "hx-trigger" (htmxCustomEventNameToText e)


instance HxTrigger Text where
    hxTrigger = makeAttribute "hx-trigger"



------------------------------
-- Swap

class HxSwap a where
    hxSwap :: a -> Attribute


instance HxSwap Swap where
    hxSwap s = makeAttribute "hx-swap" (T.pack $ show s)


instance HxSwap Text where
    hxSwap = makeAttribute "hx-swap"



------------------------------
-- OOB Swap

class HxSwapOOB a where
  hxSwapOOB :: a -> Attribute


instance HxSwapOOB Bool where
  hxSwapOOB True  = makeAttribute "hx-swap-oob" "true"
  hxSwapOOB False = makeAttribute "hx-swap-oob" "false"


-- | Use for: hx-swap-oob="outerHTML" (Defaulting to the element's ID)
instance HxSwapOOB Swap where
  hxSwapOOB s = makeAttribute "hx-swap-oob" (T.pack $ show s)


-- | Use for: hx-swap-oob="innerHTML:#some-id"
instance HxSwapOOB Text where
  hxSwapOOB = makeAttribute "hx-swap-oob"
