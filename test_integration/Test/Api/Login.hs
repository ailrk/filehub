module Test.Api.Login (spec) where


import Filehub.Auth.Types.Simple (UserRecord(..))
import Filehub.Auth.Simple (createSimpleAuthUserDB)
import Filehub.Env (Env(..))
import Filehub.Server qualified as Filehub
import Filehub.Types ( LoginForm(..) )
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai
import Web.FormUrlEncoded (ToForm(..))
import Web.FormUrlEncoded qualified as UrlFormEncoded
import Util.Env (defaultEnv)


spec :: Spec
spec = with (Filehub.application <$> patchedEnv) $ do

  ------------------------------------------------------------

  describe "Prevent access without logging-in" do
    it "should redirect to /login" do
      get "/" >>= \res -> liftIO do
        simpleStatus res `shouldBe` status307
        lookup hLocation (simpleHeaders res) `shouldBe` Just "/login"

      get "/cd?dir=x" >>= \res -> liftIO do
        simpleStatus res `shouldBe` status307
        lookup hLocation (simpleHeaders res) `shouldBe` Just "/login"

      post "/files/update?file=x" "" >>= \res -> liftIO do
        simpleStatus res `shouldBe` status307
        lookup hLocation (simpleHeaders res) `shouldBe` Just "/login"

  ------------------------------------------------------------

  describe "Login" do
    let f =  UrlFormEncoded.urlEncodeAsForm . toForm
    it "Login succeed, should redirect to /" do
      request methodPost "/login" [ (hContentType, "application/x-www-form-urlencoded") ] (f $ LoginForm "peter" "345") >>= \res -> liftIO do
          simpleStatus res `shouldBe` status200
          simpleBody res `shouldBe` ""
          lookup "HX-Redirect" (simpleHeaders res) `shouldBe` Just "/"

      request methodPost "/login" [(hContentType, "application/x-www-form-urlencoded") ] (f $ LoginForm "paul" "123") >>= \res -> liftIO do
          simpleStatus res `shouldBe` status200
          simpleBody res `shouldBe` ""
          lookup "HX-Redirect" (simpleHeaders res) `shouldBe` Just "/"

    it "Login failed, return the login form again" do
      request methodPost "/login" [ (hContentType, "application/x-www-form-urlencoded") ] (f $ LoginForm "paul" "xxx") `shouldRespondWith` 200
      request methodPost "/login" [ (hContentType, "application/x-www-form-urlencoded") ] (f $ LoginForm "peter" "xxx") `shouldRespondWith` 200
  where
    patchedEnv = defaultEnv >>= patchEnv
    patchEnv env = do
      userDB <- createSimpleAuthUserDB [UserRecord "paul" "123", UserRecord "peter" "345"]
      pure
        env { simpleAuthUserDB = userDB }
