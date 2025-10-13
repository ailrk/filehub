{-# LANGUAGE DefaultSignatures #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Debug typeclass for debug printing so we don't need to define custom Show instance.
-- If the type already has a Show instance, just dervie it with `DerivingVia`
module Text.Debug (Debug(..)) where
import GHC.Generics

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Text.Pretty.Simple (pShow)
import Data.UUID (UUID)
import Data.Time (UTCTime)
import Web.JWT (JWT)


class Debug a where
  debug :: a -> String

  debugP :: a -> LT.Text
  debugP = pShow . debug

  default debug :: (Generic a, GDebug (Rep a)) => a -> String
  debug = gdebug . from


class GDebug f where
  gdebug :: f p -> String


instance (GDebug a, GDebug b) => GDebug (a :+: b) where
  gdebug (L1 x) = gdebug x
  gdebug (R1 x) = gdebug x


instance (GDebug a, GDebug b) => GDebug (a :*: b) where
  gdebug (a :*: b) = gdebug a ++ " " ++ gdebug b


instance (GDebug a, Constructor c) => GDebug (C1 c a) where
  gdebug m@(M1 x) = conName m ++ "(" ++ gdebug x ++ ")"


instance (GDebug a) => GDebug (S1 s a) where
  gdebug (M1 x)  = gdebug x


instance (GDebug a) => GDebug (D1 s a) where
  gdebug (M1 x)  = gdebug x


instance GDebug U1 where
  gdebug U1 = ""


instance (Debug a) => GDebug (K1 i a) where
  gdebug (K1 x) = debug x


instance Debug Int where            debug = show
instance Debug Integer where        debug = show
instance Debug Float where          debug = show
instance Debug Double where         debug = show
instance Debug Bool where           debug = show
instance Debug Char where           debug = show
instance Debug Text where           debug = show . Text.unpack
instance Debug LT.Text where        debug = show . LT.unpack
instance Debug ByteString where     debug = show . ByteString.unpack
instance Debug LBS.ByteString where debug = show . LBS.unpack
instance Debug () where             debug = show
instance Debug UUID where           debug = show
instance Debug UTCTime where        debug = show
instance Debug (JWT a) where        debug = show


instance {-# OVERLAPPING #-} Debug String where  debug = show

instance (Debug a) => Debug [a] where debug = show . fmap debug
instance (Debug a) => Debug (Maybe a) where debug = show . fmap debug
instance (Debug a, Debug b) => Debug (Either b a) where debug (Left x) = debug x; debug (Right x) = debug x
instance (Debug a, Debug b) => Debug (a, b) where debug (a, b) = show (debug a, debug b)
instance (Debug a, Debug b, Debug c) => Debug (a, b, c) where debug (a, b, c) = show (debug a, debug b, debug c)

