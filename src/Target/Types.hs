{-# LANGUAGE TypeFamilies #-}
module Target.Types
  ( AnyTarget(..)
  , TargetHandler(..)
  , TargetId(..)
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


class IsTarget b where
  data family TargetBackend b
  data family Config b
  getTargetIdFromBackend :: TargetBackend b -> TargetId


-- | Existential wrapper of `Backend a`.
data AnyTarget where
  Target :: (Typeable a, IsTarget a, Debug (TargetBackend a)) => TargetBackend a -> AnyTarget


instance Debug AnyTarget where
  debug (Target backend) = debug backend


instance Eq AnyTarget where
  t1 == t2 = getTargetId t1 == getTargetId t2


data TargetHandler r = forall a. (Typeable a) => TargetHandler (TargetBackend a -> r)


targetHandler :: forall a r. (Typeable a) => (TargetBackend a -> r) -> TargetHandler r
targetHandler = TargetHandler


runTargetHandler :: AnyTarget -> TargetHandler r -> Maybe r
runTargetHandler (Target t) (TargetHandler f) = fmap f (cast t)


getTargetId :: AnyTarget -> TargetId
getTargetId (Target t) = getTargetIdFromBackend t


handleTarget :: AnyTarget -> [TargetHandler r] -> Maybe r
handleTarget target handlers = asum (map (runTargetHandler target) handlers)
