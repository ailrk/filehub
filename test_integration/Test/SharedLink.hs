module Test.SharedLink (spec) where

import Filehub.Prelude
import Data.Time (getCurrentTime, addUTCTime)
import Data.ClientPath (AbsPath(..))
import Data.File (File(..), FileInfo, defaultFileInfo)
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Filehub.SharedLink
import Target.Types (TargetId(..))
import Test.Hspec


spec :: Spec
spec = do

  describe "mkSharedLinkHash" do

    it "produces a non-empty hash" do
      let h = mkSharedLinkHash (bs_packChars "hello")
      let SharedLinkHash t = h
      txt_null t `shouldBe` False

    it "result is at most 7 characters" do
      let SharedLinkHash t = mkSharedLinkHash (bs_packChars "some input")
      txt_length t `shouldSatisfy` (<= 7)

    it "is deterministic — same input yields the same hash" do
      mkSharedLinkHash (bs_packChars "hello") `shouldBe` mkSharedLinkHash (bs_packChars "hello")

    it "different inputs produce different hashes" do
      mkSharedLinkHash (bs_packChars "hello") `shouldNotBe` mkSharedLinkHash (bs_packChars "world")
      mkSharedLinkHash (bs_packChars "")     `shouldNotBe` mkSharedLinkHash (bs_packChars "x")


  describe "createSharedLink" do

    it "revoked starts as False" do
      link <- createSharedLink sampleFile dummyTargetId Indefinite False Nothing
      link.revoked `shouldBe` False

    it "accessCount starts at 0" do
      link <- createSharedLink sampleFile dummyTargetId Indefinite False Nothing
      link.accessCount `shouldBe` 0

    it "linkType and readonly match the arguments" do
      link <- createSharedLink sampleFile dummyTargetId SingleUse True Nothing
      link.linkType `shouldBe` SingleUse
      link.readonly `shouldBe` True

    it "hash is non-empty and at most 7 characters" do
      link <- createSharedLink sampleFile dummyTargetId Indefinite False Nothing
      let SharedLinkHash h = link.hash
      txt_length h `shouldSatisfy` (\n -> n >= 1 && n <= 7)

    it "two links for the same file get different hashes (random salt)" do
      l1 <- createSharedLink sampleFile dummyTargetId Indefinite False Nothing
      l2 <- createSharedLink sampleFile dummyTargetId Indefinite False Nothing
      l1.hash `shouldNotBe` l2.hash

    it "passcode is stored as supplied" do
      let pc = Just (SharedLinkPasscode "secret")
      link <- createSharedLink sampleFile dummyTargetId Indefinite False pc
      link.passcode `shouldBe` pc


  describe "SharedLinkPool" do

    it "empty pool returns Nothing on lookup" do
      pool <- newShareLinkPool
      result <- lookupSharedLink (SharedLinkHash "nosuchkey") pool
      isNothing result `shouldBe` True

    it "SingleUse: lookup returns the link" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile dummyTargetId SingleUse False Nothing
      insertLink pool link
      result <- lookupSharedLink link.hash pool
      isJust result `shouldBe` True

    it "SingleUse: pool does not auto-consume — repeated lookups still return the link" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile dummyTargetId SingleUse False Nothing
      insertLink pool link
      _ <- lookupSharedLink link.hash pool
      result <- lookupSharedLink link.hash pool
      isJust result `shouldBe` True

    it "Indefinite: repeated lookups all return the link" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile dummyTargetId Indefinite False Nothing
      insertLink pool link
      r1 <- lookupSharedLink link.hash pool
      r2 <- lookupSharedLink link.hash pool
      isJust r1 `shouldBe` True
      isJust r2 `shouldBe` True

    it "Indefinite: accessCount increments on each lookup" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile dummyTargetId Indefinite False Nothing
      insertLink pool link
      _ <- lookupSharedLink link.hash pool
      _ <- lookupSharedLink link.hash pool
      m <- poolMap pool
      case map_lookup link.hash m of
        Nothing -> expectationFailure "link disappeared from pool"
        Just l  -> l.accessCount `shouldBe` 2

    it "Expiry: past expiry date returns Nothing" do
      pool <- newShareLinkPool
      now  <- getCurrentTime
      let past = addUTCTime (-3600) now
      link <- createSharedLink sampleFile dummyTargetId (Expiry past) False Nothing
      insertLink pool link
      result <- lookupSharedLink link.hash pool
      isNothing result `shouldBe` True

    it "Expiry: future expiry date returns the link" do
      pool <- newShareLinkPool
      now  <- getCurrentTime
      let future = addUTCTime 3600 now
      link <- createSharedLink sampleFile dummyTargetId (Expiry future) False Nothing
      insertLink pool link
      result <- lookupSharedLink link.hash pool
      isJust result `shouldBe` True

    it "revokeSharedLink sets revoked = True in the pool" do
      pool <- newShareLinkPool
      link <- createSharedLink sampleFile dummyTargetId Indefinite False Nothing
      insertLink pool link
      revokeSharedLink link.hash pool
      m <- poolMap pool
      case map_lookup link.hash m of
        Nothing -> expectationFailure "link disappeared from pool after revocation"
        Just l  -> l.revoked `shouldBe` True


-- ---------------------------------------------------------------------------
-- Helpers

sampleFile :: FileInfo
sampleFile = defaultFileInfo { path = AbsPath "/files/sample.txt" }

dummyTargetId :: TargetId
dummyTargetId = TargetId UUID.nil

insertLink :: MonadIO m => SharedLinkPool -> SharedLink -> m ()
insertLink (SharedLinkPool poolVar) link =
  atomically $ modifyTVar' poolVar (map_insert link.hash link)

poolMap :: MonadIO m => SharedLinkPool -> m (Map.Map SharedLinkHash SharedLink)
poolMap (SharedLinkPool poolVar) = atomically (readTVar poolVar)
