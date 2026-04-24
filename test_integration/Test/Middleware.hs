module Test.Middleware (spec) where

import Data.ByteString.Char8 qualified as ByteString
import Filehub.Server qualified as Filehub
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai
import Util.Env (defaultEnv)


spec :: Spec
spec = with (Filehub.application <$> defaultEnv) $ do
  describe "Session middlware" $ do
      -- Request without session cookie should always get a new session id
      it "Should have Set-Cookie header with a new session id" do
        s <- get "/healthz"
        liftIO $ do
          simpleStatus s `shouldBe` status200
          case lookup hSetCookie $ simpleHeaders s of
            Just raw -> ByteString.unpack raw `shouldContain` "sessionId="
            Nothing -> expectationFailure ("No Set-Cookie header. " <> show (simpleHeaders s))
