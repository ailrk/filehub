{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Filehub.Template.Htmx where

import Filehub.Routes
import Filehub.Routes.LinkConf (LinkConf(..), AsLinkConf, apiLinksConf)
import Filehub.Routes.Links (apiLinks)
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)
import Lucid (Attribute)
import Lucid.Htmx (hxTarget, HxSwap (..), HtmxCustomEvent (..), HxOn (..))
import Servant (Link, AsLink)


-- | Computes a clean, flat tuple type from a nested function signature
type family ArgsTuple f where
  ArgsTuple Link                       = ()
  ArgsTuple (a -> Link)                = a
  ArgsTuple (a -> b -> Link)           = (a, b)
  ArgsTuple (a -> b -> c -> Link)      = (a, b, c)
  ArgsTuple (a -> b -> c -> d -> Link) = (a, b, c, d)


-- | Maps our clean flat tuple arguments back onto the underlying function
class RunArgs f where
  runArgs :: f -> ArgsTuple f -> Link

instance RunArgs Link where
  runArgs lnk () = lnk

instance RunArgs (a -> Link) where
  runArgs f a = f a

instance RunArgs (a -> b -> Link) where
  runArgs f (a, b) = f a b

instance RunArgs (a -> b -> c -> Link) where
  runArgs f (a, b, c) = f a b c

instance RunArgs (a -> b -> c -> d -> Link) where
  runArgs f (a, b, c, d) = f a b c d


asHtmx
  :: forall (field :: Symbol) urlFunc
   . ( HasField field (Api (AsLink Link)) urlFunc
     , RunArgs urlFunc
     , HasField field (Api AsLinkConf) LinkConf
     )
  => (Link -> Attribute)  -- ^ HTMX Verb (e.g., hxPost, hxGet)
  -> ArgsTuple urlFunc    -- ^ The arguments for the path parameter tuple (use () if none)
  -> [Attribute]          -- ^ Extra attributes
  -> [Attribute]
asHtmx hxVerb args extraAttrs =
  let
    rawUrlFunc = getField @field apiLinks
    url        = runArgs rawUrlFunc args
    config     = getField @field apiLinksConf
  in
    mconcat
      [ [ hxVerb url
        , hxSwap config.hxSwap
        ]
      , case config.hxTarget of
          Just t  -> [ hxTarget t ]
          Nothing -> []
      , case config.hxOn of
          Just (evt, handler) -> [ hxOn (HtmxCustomEvent evt) handler ]
          Nothing             -> []
      , extraAttrs
      ]
