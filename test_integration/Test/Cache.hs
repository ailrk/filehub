module Test.Cache (spec) where

import Test.Hspec
import Filehub.Monad (runFilehub, Filehub)
import Util.Env (defaultEnv)
import UnliftIO (MonadIO(..), bracket)
import Cache.Key (mkCacheKey, SomeCacheKey(..))
import Control.Service.Cache (MonadCache(..))
import Filehub.Env (Env)
import UnliftIO.Concurrent (threadDelay)


withEnv :: SpecWith Env -> Spec
withEnv = around (bracket defaultEnv (\_ -> pure ()))


testFilehub :: Env -> Filehub () -> Expectation
testFilehub env action = do
  res <- runFilehub env action
  case res of
    Left err -> expectationFailure (show err)
    Right () -> pure ()


spec :: Spec
spec =
  describe "Cache" $ withEnv $ do
    it "returns Nothing for a missing key" \env ->
      testFilehub env do
        let key = mkCacheKey @Int ["foo"]
        result <- cacheLookup key
        liftIO $ result `shouldBe` Nothing

    it "makes the value retrievable via cacheLookup" \env -> do
      testFilehub env do
        let key = mkCacheKey @Int ["foo"]
        cacheInsert key [] Nothing (42 :: Int)
        result <- cacheLookup key
        liftIO $ result `shouldBe` Just 42

    it "removes the value so cacheLookup returns Nothing" \env -> do
      testFilehub env $ do
        let key = mkCacheKey @Int ["foo"]
        cacheInsert key [] Nothing (42 :: Int)
        cacheDelete (SomeCacheKey key)
        result <- cacheLookup key
        liftIO $ result `shouldBe` Nothing

    it "clears all entries so cacheLookup returns Nothing" \env -> do
      testFilehub env $ do
        let key = mkCacheKey @Int ["foo"]
        cacheInsert key [] Nothing (99 :: Int)
        cacheFlush
        result <- cacheLookup key
        liftIO $ result `shouldBe` Nothing

    it "cache value correctly" \env -> do
      testFilehub env $ do
        let key = mkCacheKey @Int ["foo"]
        -- Initially cache is empty
        r1 <- cacheLookup key
        liftIO (r1 `shouldBe` Nothing)

    it "cache expire correctly" \env -> do
      testFilehub env $ do
        let key = mkCacheKey @Int ["expire"]
        -- Insert value with TTL of 0.1 seconds
        cacheInsert key [] (Just 0.1) (123 :: Int)

        -- Immediately exists
        r1 <- cacheLookup key
        liftIO (r1 `shouldBe` Just 123)

        -- Wait for 150ms, longer than TTL
        liftIO $ threadDelay (150 * 1000)

        -- Should have expired
        r2 <- cacheLookup key
        liftIO (r2 `shouldBe` Nothing)
