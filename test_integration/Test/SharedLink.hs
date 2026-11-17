module Test.SharedLink (spec) where

import Data.ByteString.Char8 qualified as BC
import Data.ClientPath (AbsPath(..))
import Data.File (File(..), FileInfo, defaultFileInfo)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as Text
import Data.Time (addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Filehub.SharedLink
import Target.Types (TargetId(..))
import Test.Hspec
import UnliftIO (MonadIO(..))
import UnliftIO.STM (atomically, modifyTVar', readTVar)


spec :: Spec
spec = do

  describe "mkSharedLinkHash" do

    it "produces a non-empty hash" do
      let h = mkSharedLinkHash (BC.pack "hello")
      let SharedLinkHash t = h
      Text.null t `shouldBe` False

    it "result is at most 7 characters" do
      let SharedLinkHash t = mkSharedLinkHash (BC.pack "some input")
      Text.length t `shouldSatisfy` (<= 7)

    it "is deterministic — same input yields the same hash" do
      mkSharedLinkHash (BC.pack "hello") `shouldBe` mkSharedLinkHash (BC.pack "hello")

    it "different inputs produce different hashes" do
      mkSharedLinkHash (BC.pack "hello") `shouldNotBe` mkSharedLinkHash (BC.pack "world")
      mkSharedLinkHash (BC.pack "")     `shouldNotBe` mkSharedLinkHash (BC.pack "x")


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
      Text.length h `shouldSatisfy` (\n -> n >= 1 && n <= 7)

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
      case Map.lookup link.hash m of
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
      case Map.lookup link.hash m of
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
  atomically $ modifyTVar' poolVar (Map.insert link.hash link)

poolMap :: MonadIO m => SharedLinkPool -> m (Map.Map SharedLinkHash SharedLink)
poolMap (SharedLinkPool poolVar) = atomically (readTVar poolVar)
