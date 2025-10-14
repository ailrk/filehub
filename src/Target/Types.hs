{-# LANGUAGE TypeFamilies #-}
module Target.Types
  ( Target(..)
  , TargetHandler(..)
  , TargetId(..)
  , TargetBackend
  , IsTarget(..)
  , targetHandler
  , runTargetHandler
  , getTargetId
  , handleTarget
  , targetIdBuilder
  )
  where

import Control.Applicative (asum)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.Generics.Labels ()
import Data.Hashable (Hashable)
import Data.Typeable (Typeable, cast)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Lens.Micro.Platform ()
import Network.URI.Encode qualified as URI.Encode
import Prelude hiding (readFile, writeFile)
import Servant (ToHttpApiData (..), FromHttpApiData (..))
import Text.Debug (Debug(..))


newtype TargetId = TargetId UUID
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable)


instance Debug TargetId where debug = show


instance ToHttpApiData TargetId where
  toUrlPiece (TargetId p) = toUrlPiece p


instance FromHttpApiData TargetId where
  parseUrlPiece p = TargetId <$> parseUrlPiece (URI.Encode.decodeText p)


targetIdBuilder :: TargetId -> Builder
targetIdBuilder (TargetId targetId) =  Builder.byteString . UUID.toASCIIBytes $ targetId


data family TargetBackend b


class IsTarget b where
  getTargetIdFromBackend :: TargetBackend b -> TargetId


-- | Existential wrapper of `Backend a`.
data Target where
  Target :: (Typeable a, IsTarget a, Debug (TargetBackend a)) => TargetBackend a -> Target


instance Debug Target where
  debug (Target backend)= debug backend


instance Eq Target where
  t1 == t2 = getTargetId t1 == getTargetId t2


data TargetHandler r = forall a. (Typeable a) => TargetHandler (TargetBackend a -> r)


targetHandler :: forall a r. (Typeable a) => (TargetBackend a -> r) -> TargetHandler r
targetHandler = TargetHandler


runTargetHandler :: Target -> TargetHandler r -> Maybe r
runTargetHandler (Target t) (TargetHandler f) = fmap f (cast t)


getTargetId :: Target -> TargetId
getTargetId (Target t) = getTargetIdFromBackend t


handleTarget :: Target -> [TargetHandler r] -> Maybe r
handleTarget target handlers = asum (map (runTargetHandler target) handlers)
