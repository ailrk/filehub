module Data.ClientPathSpec (spec) where

import Data.Char (isPrint)
import Data.ClientPath qualified as ClientPath
import Filehub.Types (ClientPath(..), RawClientPath(..))
import Network.URI.Encode qualified as URI
import System.FilePath ((</>), normalise)
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec =
  describe "ClientPath (property-based)" do
    it "fromClientPath . toClientPath = id" do
      property $ \(RelPath p) -> do
        let absPath = mkAbsPath p
        ClientPath.fromClientPath rootPath (ClientPath.toClientPath rootPath absPath) === absPath

    it "fromRawClientPath . toRawClientPath = id" do
      property $ \(RelPath p) -> do
        let absPath = mkAbsPath p
        ClientPath.fromRawClientPath rootPath (ClientPath.toRawClientPath rootPath absPath) === absPath

    it "ClientPath must be percent encoded" do
      property $ \(RelPath p) -> do
        let absPath = mkAbsPath p
        let ClientPath s = ClientPath.toClientPath rootPath absPath
        URI.encode (URI.decode s) == s

    it "RawClientPath should not start with /" do
      property $ \(RelPath p) -> do
        let absPath = mkAbsPath p
        let RawClientPath s = ClientPath.toRawClientPath rootPath absPath
        case s of
          '/':_ -> False
          _ -> True
  where
    mkAbsPath p = normalise $ rootPath </> dropWhile (== '/') p
    rootPath = "/root"


-- | Relative, printable paths
newtype RelPath = RelPath FilePath
  deriving (Show, Eq)


instance Arbitrary RelPath where
  arbitrary = RelPath <$> genSafePath


-- Generate relative, printable paths with slashes
genSafePath :: Gen FilePath
genSafePath = do
  segments <- listOf1 genSegment
  return $ foldr1 (\a b -> a ++ "/" ++ b) segments
  where
    genSegment = listOf1 (suchThat arbitrary (\c -> isPrint c && c /= '/'))
