module Test.SharedLink (spec) where

import Data.ByteString.Char8 qualified as BC
import Data.ClientPath (AbsPath(..))
import Data.File (File(..), FileInfo, defaultFileInfo)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as Text
import Data.Time (addUTCTime, getCurrentTime)
import Filehub.SharedLink
import Test.Hspec
import UnliftIO (MonadIO(..))
import UnliftIO.STM (atomically, modifyTVar', readTVar)


spec :: Spec
spec = do

  describe "toBase62" do

    it "toBase62 0 == \"0\"" do
      toBase62 0 `shouldBe` "0"

    it "toBase62 62 == \"10\" (one full base-62 digit)" do
      toBase62 62 `shouldBe` "10"

    it "toBase62 61 is the last single-character value" do
      length (toBase62 61) `shouldBe` 1

    it "result only contains base-62 characters" do
      let isBase62 c = c `elem` (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])
      all isBase62 (toBase62 123456789) `shouldBe` True

    it "toBase62 is injective on small positive integers" do
      let values = [0..200] :: [Integer]
      length (map toBase62 values) `shouldBe` length (map toBase62 values)
      -- distinct inputs must produce distinct outputs
      let encoded = map toBase62 values
      length encoded `shouldBe` length (foldr (\x acc -> if x `elem` acc then acc else x : acc) [] encoded)


  describe "shortHash" do

    it "is deterministic — same input yields the same hash" do
      shortHash (BC.pack "hello") `shouldBe` shortHash (BC.pack "hello")

    it "has length at most 7" do
      Text.length (shortHash (BC.pack "some input")) `shouldSatisfy` (<= 7)

    it "different inputs produce different hashes" do
      shortHash (BC.pack "hello") `shouldNotBe` shortHash (BC.pack "world")
      shortHash (BC.pack "")     `shouldNotBe` shortHash (BC.pack "x")


  describe "createSharedLink" do

    it "revoked starts as False" do
      link <- createSharedLink sampleFile Indefinite False Nothing
      link.revoked `shouldBe` False

    it "accessCount starts at 0" do
      link <- createSharedLink sampleFile Indefinite False Nothing
      link.accessCount `shouldBe` 0

    it "linkType and readonly match the arguments" do
      link <- createSharedLink sampleFile SingleUse True Nothing
      link.linkType `shouldBe` SingleUse
      link.readonly `shouldBe` True

    it "hash is derived from the file path" do
      link <- createSharedLink sampleFile Indefinite False Nothing
      link.hash `shouldBe` mkSharedLinkHash (BC.pack "/files/sample.txt")

    it "passcode is stored as supplied" do
      let pc = Just (SharedLinkPasscode "secret")
      link <- createSharedLink sampleFile Indefinite False pc
      link.passcode `shouldBe` pc


  describe "SharedLinkPool" do

    it "empty pool returns Nothing on lookup" do
      pool <- newShareLinkPool
      result <- lookupSharedLink (SharedLinkHash "nosuchkey") pool
      isNothing result `shouldBe` True

    it "SingleUse: first access returns the link" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile SingleUse False Nothing
      insertLink pool link
      result <- lookupSharedLink link.hash pool
      isJust result `shouldBe` True

    it "SingleUse: link is consumed — second access returns Nothing" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile SingleUse False Nothing
      insertLink pool link
      _ <- lookupSharedLink link.hash pool
      result <- lookupSharedLink link.hash pool
      isNothing result `shouldBe` True

    it "Indefinite: repeated lookups all return the link" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile Indefinite False Nothing
      insertLink pool link
      r1 <- lookupSharedLink link.hash pool
      r2 <- lookupSharedLink link.hash pool
      isJust r1 `shouldBe` True
      isJust r2 `shouldBe` True

    it "Indefinite: accessCount increments on each lookup" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile Indefinite False Nothing
      insertLink pool link
      _ <- lookupSharedLink link.hash pool
      _ <- lookupSharedLink link.hash pool
      m <- poolMap pool
      case Map.lookup link.hash m of
        Nothing -> expectationFailure "link disappeared from pool"
        Just l  -> l.accessCount `shouldBe` 2

    it "Expiry: past expiry date returns Nothing" do
      pool <- newShareLinkPool
      now  <- getCurrentTime
      let past = addUTCTime (-3600) now
      link <- createSharedLink sampleFile (Expiry past) False Nothing
      insertLink pool link
      result <- lookupSharedLink link.hash pool
      isNothing result `shouldBe` True

    it "Expiry: future expiry date returns the link" do
      pool <- newShareLinkPool
      now  <- getCurrentTime
      let future = addUTCTime 3600 now
      link <- createSharedLink sampleFile (Expiry future) False Nothing
      insertLink pool link
      result <- lookupSharedLink link.hash pool
      isJust result `shouldBe` True

    it "revokeSharedLink sets revoked = True in the pool" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile Indefinite False Nothing
      insertLink pool link
      revokeSharedLink link.hash pool
      m <- poolMap pool
      case Map.lookup link.hash m of
        Nothing -> expectationFailure "link disappeared from pool after revocation"
        Just l  -> l.revoked `shouldBe` True


-- ---------------------------------------------------------------------------
-- Helpers

sampleFile :: FileInfo
sampleFile = defaultFileInfo { path = AbsPath "/files/sample.txt" }


insertLink :: MonadIO m => SharedLinkPool -> SharedLink -> m ()
insertLink (SharedLinkPool poolVar) link =
  atomically $ modifyTVar' poolVar (Map.insert link.hash link)


poolMap :: MonadIO m => SharedLinkPool -> m (Map.Map SharedLinkHash SharedLink)
poolMap (SharedLinkPool poolVar) = atomically (readTVar poolVar)
