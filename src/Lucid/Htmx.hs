{-# LANGUAGE OverloadedStrings #-}

module Lucid.Htmx where

import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttribute)
import Filehub.Links (linkToText)
import Servant (Link)
import Data.Text qualified as Text


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
    show (Every t) = "every " <> Text.unpack t


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
    | CustomEvent Text
    deriving (Eq)


instance Show HtmxEvent where
    show BeforeRequest   = "htmx:beforeRequest"
    show BeforeSend      = "htmx:beforeSend"
    show AfterOnLoad     = "htmx:afterOnLoad"
    show AfterRequest    = "htmx:afterRequest"
    show AfterSettle     = "htmx:afterSettle"
    show AfterSwap       = "htmx:afterSwap"
    show OobBeforeSwap   = "htmx:oobBeforeSwap"
    show OobAfterSwap    = "htmx:oobAfterSwap"
    show ResponseError   = "htmx:responseError"
    show SendError       = "htmx:sendError"
    show (CustomEvent t) = Text.unpack t


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
      let eventName = Text.pack (show event)
          cleanName = Text.replace "htmx:" "" eventName
      in makeAttribute ("hx-on::" <> cleanName) script


instance HxOn Trigger where
  hxOn event script =
      let eventName = Text.pack (show event)
          cleanName = Text.replace "htmx:" "" eventName
      in makeAttribute ("hx-on::" <> cleanName) script


------------------------------
-- Trigger

class HxTrigger a where
    hxTrigger :: a -> Attribute


instance HxTrigger Trigger where
    hxTrigger t = makeAttribute "hx-trigger" (Text.pack $ show t)


instance HxTrigger HtmxEvent where
    hxTrigger e = makeAttribute "hx-trigger" (Text.pack $ show e)


instance HxTrigger Text where
    hxTrigger = makeAttribute "hx-trigger"



------------------------------
-- Swap

class HxSwap a where
    hxSwap :: a -> Attribute


instance HxSwap Swap where
    hxSwap s = makeAttribute "hx-swap" (Text.pack $ show s)


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
  hxSwapOOB s = makeAttribute "hx-swap-oob" (Text.pack $ show s)


-- | Use for: hx-swap-oob="innerHTML:#some-id"
instance HxSwapOOB Text where
  hxSwapOOB = makeAttribute "hx-swap-oob"
