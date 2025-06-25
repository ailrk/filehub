module Main where


import Test.Hspec
import Test.QuickCheck
import Filehub.Types (ClientPath(..), RawClientPath(..))
import Filehub.ClientPath qualified as ClientPath
import Data.Char (isPrint)
import System.FilePath.Posix ((</>))
import Network.URI.Encode qualified as URI


main :: IO ()
main = hspec $ do
  describe "ClientPath" clientPathSpec


clientPathSpec :: Spec
clientPathSpec =
  describe "ClientPath (property-based)" $ do
    it "fromClientPath . toClientPath = id" $
      property $ \(RelPath p) ->
        let absPath = mkAbsPath p
         in ClientPath.fromClientPath rootPath (ClientPath.toClientPath rootPath absPath) === absPath

    it "fromRawClientPath . toRawClientPath = id" $
      property $ \(RelPath p) ->
        let absPath = mkAbsPath p
         in ClientPath.fromRawClientPath rootPath (ClientPath.toRawClientPath rootPath absPath) === absPath

    it "ClientPath must be percent encoded" $ do
      property $ \(RelPath p) ->
        let absPath = mkAbsPath p
            ClientPath s = ClientPath.toClientPath rootPath absPath
         in URI.encode (URI.decode s) == s

    it "RawClientPath should not start with /" $ do
      property $ \(RelPath p) ->
        let absPath = mkAbsPath p
            RawClientPath s = ClientPath.toRawClientPath rootPath absPath
         in case s of
              '/':_ -> False
              _ -> True
  where
    mkAbsPath p = rootPath </> dropWhile (== '/') p
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
