module Data.ClientPathSpec (spec) where

import Data.Char (isPrint)
import Data.ClientPath qualified as ClientPath
import Data.ClientPath (AbsPath(..), Root(..), newAbsPath, (<./>))
import Filehub.Types (ClientPath(..), RawClientPath(..))
import Network.URI.Encode qualified as URI
import System.FilePath ((</>), normalise)
import Test.Hspec
import Test.QuickCheck
import Data.Coerce (coerce)


spec :: Spec
spec =
  describe "ClientPath (property-based)" do
    it "fromClientPath . toClientPath = id" do
      property $ \(RelPath p) -> do
        let absPath = mkAbsPath p
        ClientPath.fromClientPath (Root (AbsPath rootPath)) (ClientPath.toClientPath (Root (AbsPath rootPath)) (AbsPath absPath)) === AbsPath absPath

    it "fromRawClientPath . toRawClientPath = id" do
      property $ \(RelPath p) -> do
        let absPath = mkAbsPath p
        ClientPath.fromRawClientPath rootPath (ClientPath.toRawClientPath rootPath absPath) === absPath

    it "ClientPath must be percent encoded" do
      property $ \(RelPath p) -> do
        let absPath = mkAbsPath p
        let ClientPath s = ClientPath.toClientPath (Root (AbsPath rootPath)) (AbsPath absPath)
        URI.encode (URI.decode s) == s

    it "RawClientPath should not start with /" do
      property $ \(RelPath p) -> do
        let absPath = mkAbsPath p
        let RawClientPath s = ClientPath.toRawClientPath rootPath absPath
        case s of
          '/':_ -> False
          _ -> True

    it "toClientPath . fromClientPath = id" do
      property $ \(RelPath p) ->
        let absPath    = mkAbsPath p
            root'      = Root (AbsPath rootPath)
            clientPath = ClientPath.toClientPath root' (AbsPath absPath)
        in ClientPath.toClientPath root' (ClientPath.fromClientPath root' clientPath) === clientPath

    it "distinct abs paths produce distinct ClientPaths" do
      property $ \(RelPath p1) (RelPath p2) ->
        let abs1  = mkAbsPath p1
            abs2  = mkAbsPath p2
            root' = Root (AbsPath rootPath)
        in abs1 /= abs2 ==>
           ClientPath.toClientPath root' (AbsPath abs1) /= ClientPath.toClientPath root' (AbsPath abs2)

    it "newAbsPath accepts absolute paths" do
      newAbsPath "/root/file" `shouldBe` Just (AbsPath "/root/file")

    it "newAbsPath rejects relative paths" do
      newAbsPath "relative/path" `shouldBe` Nothing
      newAbsPath "file.txt" `shouldBe` Nothing

    it "(<./>) appends paths like </> on the underlying FilePath" do
      AbsPath "/root" <./> AbsPath "sub/file" `shouldBe` AbsPath "/root/sub/file"
  where
    mkAbsPath p = (normalise $ coerce rootPath </> dropWhile (== '/') p)
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
