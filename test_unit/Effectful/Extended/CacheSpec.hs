module Effectful.Extended.CacheSpec (spec) where

import Test.Hspec
import Cache.InMemory qualified as InMemory
import Effectful (runEff, liftIO)
import Effectful.Extended.Cache
import Prelude hiding (lookup)
import Effectful.Log (runLog, LogLevel (..), Logger, mkLogger, LogMessage (lmMessage))
import Control.Monad (join)
import Data.Text qualified as Text
import GHC.Conc.IO (threadDelay)

spec :: Spec
spec = describe "Effectful.Extended.Cache" do
  specInMemory


specInMemory :: Spec
specInMemory = describe "Cache.InMemory" do
  it "cache value correctly" do
    cache <- InMemory.new 10
    logger <- nullLogger 2
    join . runEff . runLog "cache" logger LogTrace . runCacheInMemory cache $ do
      let key = mkCacheKey @Int ["foo"]
      -- Initially cache is empty
      r1 <- lookup key
      liftIO (r1 `shouldBe` Nothing)

      -- Insert value
      insert key [] Nothing (42 :: Int)

      -- Lookup returns inserted value
      r2 <- lookup key
      liftIO (r2 `shouldBe` Just 42)

      -- Delete value
      delete key

      -- Lookup returns Nothing after deletion
      r3 <- lookup key
      liftIO (r3 `shouldBe` Nothing)

      -- Insert again and flush
      insert key [] Nothing (99 :: Int)
      flush

      -- Lookup returns Nothing after flush
      r4 <- lookup key
      pure (r4 `shouldBe` Nothing)

  it "cache expire correctly" do
    cache <- InMemory.new 10
    logger <- nullLogger 2
    join . runEff . runLog "cache" logger LogTrace . runCacheInMemory cache $ do
      let key = mkCacheKey @Int ["expire"]
      -- Insert value with TTL of 0.1 seconds
      insert key [] (Just 0.1) (123 :: Int)

      -- Immediately exists
      r1 <- lookup key
      liftIO (r1 `shouldBe` Just 123)

      -- Wait for 150ms, longer than TTL
      liftIO $ threadDelay (150 * 1000)

      -- Should have expired
      r2 <- lookup key
      pure (r2 `shouldBe` Nothing)


nullLogger :: Int -> IO Logger
nullLogger n = mkLogger "" $ \msg -> do
  putStrLn (Text.unpack $ (Text.replicate n "    " ) <> msg.lmMessage)
