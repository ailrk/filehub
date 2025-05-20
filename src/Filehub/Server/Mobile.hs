{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Mobile where

import Filehub.Monad ( Filehub )
import Filehub.Types
    ( SessionId(..),
      SessionId(..))
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Prelude hiding (readFile)



index :: SessionId -> Filehub (Html ())
index sessionId =
  undefined


view :: SessionId -> Filehub (Html ())
view sessionId = do
  undefined
