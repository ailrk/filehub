module Test.RateLimit (spec) where

import Control.Monad (replicateM_)
import Filehub.Env (Env(..))
import Filehub.RateLimit (RateLimiters(..))
import Filehub.RateLimit qualified as RateLimit
import Filehub.Server qualified as Filehub
import Network.HTTP.Types.Status (status429)
import Network.Wai.Test (simpleStatus)
import Test.Hspec
import Test.Hspec.Wai
import Util.Env (defaultEnv)


spec :: Spec
spec = do

  describe "RateLimit.check" $
    it "allows exactly maxRequests then blocks" $ do
      rl      <- RateLimit.new 5 10
      results <- mapM (\_ -> RateLimit.check rl "ip") [1 .. 8 :: Int]
      length (filter id results)  `shouldBe` 5
      length (filter not results) `shouldBe` 3

  with mkApp $
    describe "rate limit middleware" $
      it "returns 429 after global limit is exceeded" $ do
        replicateM_ 3 $ get "/healthz"
        r <- get "/healthz"
        liftIO $ simpleStatus r `shouldBe` status429

  where
    mkApp = do
      env     <- defaultEnv
      tightRl <- RateLimit.new 3 60
      let rls  = env.rateLimiters
          env' = env { rateLimiters = rls { global = tightRl } }
      pure (Filehub.application env')
