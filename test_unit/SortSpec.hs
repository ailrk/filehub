{-# LANGUAGE LambdaCase #-}
module SortSpec (spec) where

import Filehub.Prelude
import Data.ClientPath (AbsPath(..))
import Data.File (File(..), FileType(..), FileInfo, defaultFileInfo)
import Filehub.Sort
import System.FilePath (takeFileName)
import Test.Hspec
import Test.QuickCheck
import Web.HttpApiData (toUrlPiece, parseUrlPiece)


-- ---------------------------------------------------------------------------
-- Generators

epoch :: UTCTime
epoch = read "2000-01-01 00:00:00 UTC"

genUTCTime :: Gen UTCTime
genUTCTime = do
  s <- choose (0, 1000000 :: Int)
  pure $ addUTCTime (fromIntegral s) epoch

genName :: Gen String
genName = listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']))

genFileInfo :: Gen FileInfo
genFileInfo = do
  name    <- genName
  content <- elements [Regular, Dir]
  mtime   <- frequency [(1, pure Nothing), (4, Just <$> genUTCTime)]
  size    <- frequency [(1, pure Nothing), (4, Just . getNonNegative <$> arbitrary)]
  pure defaultFileInfo
    { path    = AbsPath ("/root/" <> name)
    , content = content
    , mtime   = mtime
    , size    = size
    }

newtype Files = Files [FileInfo]

instance Show Files where
  show (Files fs) = "Files (" <> show (length fs) <> " items)"

instance Arbitrary Files where
  arbitrary = Files <$> listOf genFileInfo

instance Arbitrary SortFileBy where
  arbitrary = elements [ByNameUp, ByNameDown, ByModifiedUp, ByModifiedDown, BySizeUp, BySizeDown]

-- ---------------------------------------------------------------------------
-- Helpers

isDir :: FileInfo -> Bool
isDir f = f.content == Dir

fileName :: FileInfo -> String
fileName f = coerce takeFileName f.path

allSortOrders :: [SortFileBy]
allSortOrders = [ByNameUp, ByNameDown, ByModifiedUp, ByModifiedDown, BySizeUp, BySizeDown]

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "SortFileBy serialization" do

    it "all constructors round-trip through URL encoding" $
      mapM_ (\s -> parseUrlPiece (toUrlPiece s) `shouldBe` Right s) allSortOrders

    it "toUrlPiece produces the expected strings" do
      toUrlPiece ByNameUp       `shouldBe` ("nameUp"       :: Text)
      toUrlPiece ByNameDown     `shouldBe` ("nameDown"     :: Text)
      toUrlPiece ByModifiedUp   `shouldBe` ("modifiedUp"   :: Text)
      toUrlPiece ByModifiedDown `shouldBe` ("modifiedDown" :: Text)
      toUrlPiece BySizeUp       `shouldBe` ("sizeUp"       :: Text)
      toUrlPiece BySizeDown     `shouldBe` ("sizeDown"     :: Text)

    it "unknown string parses as Left" $
      (parseUrlPiece @SortFileBy "badValue") `shouldSatisfy` \case
        Left _  -> True
        Right _ -> False


  describe "sortFiles structural invariants" do

    it "empty list is preserved for all sort orders" $
      mapM_ (\s -> sortFiles s [] `shouldBe` []) allSortOrders

    it "singleton list is unchanged for all sort orders" $ do
      let f = defaultFileInfo { path = AbsPath "/root/x" }
      mapM_ (\s -> sortFiles s [f] `shouldBe` [f]) allSortOrders

    it "sortFiles does not add or remove files" $ property $
      \(Files files) (s :: SortFileBy) ->
        length (sortFiles s files) === length files

    it "ByNameDown is the reverse of ByNameUp" $ property $
      \(Files files) ->
        sortFiles ByNameDown files === reverse (sortFiles ByNameUp files)

    it "ByModifiedDown is the reverse of ByModifiedUp" $ property $
      \(Files files) ->
        sortFiles ByModifiedDown files === reverse (sortFiles ByModifiedUp files)

    it "BySizeDown is the reverse of BySizeUp" $ property $
      \(Files files) ->
        sortFiles BySizeDown files === reverse (sortFiles BySizeUp files)

    it "sorting is idempotent for ByNameUp" $ property $
      \(Files files) ->
        let sorted = sortFiles ByNameUp files
        in sortFiles ByNameUp sorted === sorted

    it "ascending sort orders are idempotent (sortOn is stable)" $ property $
      \(Files files) ->
        -- Down variants are `reverse . sortAscending`, which is NOT idempotent
        -- when files share a sort key (reverse swaps ties, re-sort preserves them).
        -- Up variants are plain sortOn, which is always idempotent.
        all (\s -> let sorted = sortFiles s files in sortFiles s sorted == sorted)
            [ByNameUp, ByModifiedUp, BySizeUp]


  describe "ByNameUp ordering" do

    it "files of the same FileType are contiguous (no interleaving)" $ property $
      \(Files files) ->
        let sorted     = sortFiles ByNameUp files
            groupCount = length (groupBy ((==) `on` (.content)) sorted)
        in groupCount <= 2

    it "within Regular files, names are non-decreasing" $ property $
      \(Files files) ->
        let regulars = filter (not . isDir) (sortFiles ByNameUp files)
            pairs    = zip regulars (tail regulars)
        in all (\(a, b) -> fileName a <= fileName b) pairs

    it "within Dir files, names are non-decreasing" $ property $
      \(Files files) ->
        let dirs  = filter isDir (sortFiles ByNameUp files)
            pairs = zip dirs (tail dirs)
        in all (\(a, b) -> fileName a <= fileName b) pairs


  describe "ByModifiedUp ordering" do

    it "within Regular files, mtimes are non-decreasing" $ property $
      \(Files files) ->
        let regulars = filter (not . isDir) (sortFiles ByModifiedUp files)
            pairs    = zip regulars (tail regulars)
        in all (\(a, b) -> a.mtime <= b.mtime) pairs

    it "within Dir files, mtimes are non-decreasing" $ property $
      \(Files files) ->
        let dirs  = filter isDir (sortFiles ByModifiedUp files)
            pairs = zip dirs (tail dirs)
        in all (\(a, b) -> a.mtime <= b.mtime) pairs


  describe "BySizeUp ordering" do

    it "within Regular files, sizes are non-decreasing" $ property $
      \(Files files) ->
        let regulars = filter (not . isDir) (sortFiles BySizeUp files)
            pairs    = zip regulars (tail regulars)
        in all (\(a, b) -> a.size <= b.size) pairs

    it "within Dir files, sizes are non-decreasing" $ property $
      \(Files files) ->
        let dirs  = filter isDir (sortFiles BySizeUp files)
            pairs = zip dirs (tail dirs)
        in all (\(a, b) -> a.size <= b.size) pairs
