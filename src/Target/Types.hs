{-# LANGUAGE TypeFamilies #-}
module Target.Types
  ( AnyTarget(..)
  , TargetHandler(..)
  , TargetId(..)
  , HasTargetId(..)
  , IsTarget(..)
  , targetHandler
  , runTargetHandler
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
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)


newtype TargetId = TargetId UUID
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable, FromJSON, ToJSON, Debug)


instance ToHttpApiData TargetId where
  toUrlPiece (TargetId p) = toUrlPiece p


instance FromHttpApiData TargetId where
  parseUrlPiece p = TargetId <$> parseUrlPiece (URI.Encode.decodeText p)


targetIdBuilder :: TargetId -> Builder
targetIdBuilder (TargetId targetId) =  Builder.byteString . UUID.toASCIIBytes $ targetId


class HasTargetId t where
  getTargetId :: t -> TargetId


class HasTargetId (Target t) => IsTarget t where
  data family Target t
  data family Config t


-- | Existential wrapper of `Target a`.
data AnyTarget where
  AnyTarget :: (Typeable a, IsTarget a, Debug (Target a)) => Target a -> AnyTarget


instance HasTargetId AnyTarget where
  getTargetId (AnyTarget target) = getTargetId target


instance Debug AnyTarget where
  debug (AnyTarget backend) = debug backend


instance Eq AnyTarget where
  AnyTarget t1 == AnyTarget t2 = getTargetId t1 == getTargetId t2


data TargetHandler r = forall a. (Typeable a) => TargetHandler (Target a -> r)


targetHandler :: forall a r. (Typeable a) => (Target a -> r) -> TargetHandler r
targetHandler = TargetHandler


runTargetHandler :: AnyTarget -> TargetHandler r -> Maybe r
runTargetHandler (AnyTarget t) (TargetHandler f) = fmap f (cast t)


handleTarget :: AnyTarget -> [TargetHandler r] -> Maybe r
handleTarget target handlers = asum (map (runTargetHandler target) handlers)
