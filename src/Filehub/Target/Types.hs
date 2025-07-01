{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Filehub.Target.Types where

import Filehub.Target.Class (IsTarget(..))
import Data.Typeable (Typeable, cast)
import Filehub.File (File)
import Filehub.ClientPath (ClientPath)
import Lens.Micro.Platform ()
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Servant.Multipart
    ( MultipartData(..), Mem, MultipartData(..), Mem )
import Prelude hiding (readFile, writeFile)
import Data.Conduit (ConduitT)
import Conduit (ResourceT)
import Data.ByteString (ByteString)


data Storage m = Storage
  { get :: FilePath -> m File
  , read :: File -> m LBS.ByteString
  , readStream :: File -> m (ConduitT () ByteString (ResourceT IO) ())
  , write :: FilePath -> LBS.ByteString -> m ()
  , delete :: FilePath -> m ()
  , new :: FilePath -> m ()
  , newFolder :: FilePath -> m ()
  , ls :: FilePath -> m [File]
  , cd :: FilePath -> m ()
  , lsCwd :: m [File]
  , upload :: MultipartData Mem -> m ()
  , download :: ClientPath -> m (ConduitT () ByteString (ResourceT IO) ())
  , isDirectory :: FilePath -> m Bool
  }


data Target where
  Target :: (Typeable a, IsTarget a) => Backend a -> Target


data TargetHandler r = forall a. (Typeable a, IsTarget a) => TargetHandler (Backend a -> r)


targetHandler :: forall a r. (Typeable a, IsTarget a) => (Backend a -> r) -> TargetHandler r
targetHandler = TargetHandler


runTargetHandler :: Target -> TargetHandler r -> Maybe r
runTargetHandler (Target t) (TargetHandler f) = fmap f (cast t)
