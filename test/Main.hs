{-# LANGUAGE BangPatterns #-}
module Main where

import Test.Hspec
import Test.Hspec.Wai
import Test.QuickCheck
import Network.Wai.Test hiding (request)
import Filehub.ClientPath qualified as ClientPath
import Filehub.SessionPool qualified as SessionPool
import Filehub.Env (Env(..))
import Filehub.User (UserDB(..))
import Filehub.Server qualified as Filehub
import Data.Char (isPrint)
import System.FilePath ((</>), normalise)
import Network.URI.Encode qualified as URI
import Effectful (runEff)
import Data.Time (secondsToNominalDiffTime)
import Effectful.Log (LogLevel(LogTrace), Logger)
import Log (mkLogger)
import Data.UUID qualified as UUID
import Data.Maybe (fromJust)
import Filehub.Types
  ( ClientPath(..)
  , RawClientPath(..)
  , Theme(..)
  , Target(..)
  , TargetId(..))
import System.Directory (createDirectoryIfMissing, removePathForcibly, doesFileExist, doesDirectoryExist)
import Control.Monad (forM_)
import System.FilePath (takeDirectory)
import Network.HTTP.Types.Header
import Data.ByteString.Char8 qualified as ByteString
import Filehub.Target.File (Backend(..))
import Filehub.Types (LoginForm(..))
import Web.FormUrlEncoded (ToForm(..))
import Web.FormUrlEncoded qualified as UrlFormEncoded
import Network.HTTP.Types.Status
import Network.HTTP.Types (methodPost)
import Filehub.User (createUserDB)
import Filehub.Options (LoginInfo(..))
import Effectful.FileSystem (runFileSystem)


main :: IO ()
main = hspec do
  clientPathSpec
  serverSpec


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
    mkAbsPath p = normalise $ rootPath </> dropWhile (== '/') p
    rootPath = "/root"


serverSpec :: Spec
serverSpec = before setup  $ after_ teardown do
  describe "Request without session cookie should always get a new session id" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "should have Set-Cookie header with a new session id" do
        s <- get "/files/copy"
        liftIO $ do
          simpleStatus s `shouldBe` status200
          case lookup hSetCookie $ simpleHeaders s of
            Just raw -> ByteString.unpack raw `shouldContain` "sessionId="
            Nothing -> liftIO $ expectationFailure "No Set-Cookie header"


  describe "Paste when not copied" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "should fail" $ post "/files/paste" "" `shouldRespondWith` 500


  describe "Paste when nothing is selected" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "should paste nothing" do
        get "/files/copy" `shouldRespondWith` 200
        do
          res <- post "/files/paste" ""
          pure res `shouldRespondWith` 200
          liftIO $ lookup "X-Filehub-Selected-Count" (simpleHeaders res) `shouldBe` Just "0"


  describe "Paste file into the same dir" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "should should overwrite the file with the same content." do
        postHtmlForm "/table/select" [("selected", "a")] `shouldRespondWith` 200
        get "/files/copy" `shouldRespondWith` 200
        post "/files/paste" "" `shouldRespondWith` 200


  describe "Paste to a new dir" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "should paste successfully" do
        postHtmlForm "/table/select" [("selected", "a")] `shouldRespondWith` 200
        get "/files/copy" `shouldRespondWith` 200
        get "/cd?dir=dir1" `shouldRespondWith` 200
        post "/files/paste" "" `shouldRespondWith` 200
        liftIO do
          exists <- doesFileExist (root </> "dir1/a")
          exists `shouldBe` True


  describe "Paste multiple" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "should paste mutiple files successfully" do
        postHtmlForm "/table/select"
          [ ("selected", "a")
          , ("selected", "b")
          , ("selected", "dir1/x")
          ] `shouldRespondWith` 200
        get "/files/copy" `shouldRespondWith` 200
        get "/cd?dir=dir2" `shouldRespondWith` 200
        post "/files/paste" "" `shouldRespondWith` 200
        liftIO do
          exists <- allPathsExist root ["dir2/a", "dir2/b", "dir2/x"]
          exists `shouldBe` True


  describe "Paste a dir" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "should fail, we don't support pasting directory" do
        postHtmlForm "/table/select" [("selected", "/dir1")] `shouldRespondWith` 200
        get "/files/copy" `shouldRespondWith` 200
        get "/cd?dir=dir2" `shouldRespondWith` 200
        post "/files/paste" "" `shouldRespondWith` 500


  describe "Delete a single file" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "file a should be deleted" do
        delete "/files/delete?file=a" `shouldRespondWith` 200
        liftIO do
          exists <- doesFileExist (root </> "a")
          exists `shouldBe` False
        liftIO do
          exists <- allPathsExist root ["b", "dir1/x", "dir2/subdir/y"]
          exists `shouldBe` True


  describe "Delete a folder" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "directory dir2 sould be deleted" do
        delete "/files/delete?file=dir2" `shouldRespondWith` 200
        liftIO do
          exists <- doesDirectoryExist (root </> "dir2")
          exists `shouldBe` False
        liftIO do
          exists <- allPathsExist root ["a", "b", "dir1/x"]
          exists `shouldBe` True


  describe "/files/new" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "should create a file `new` in root directory" do
        postHtmlForm "/files/new" [("new-file", "new")] `shouldRespondWith` 200
        liftIO do
          exists <- doesFileExist (root </> "new")
          exists `shouldBe` True

      it "should create a file `new` in dir2/" do
        get "/cd?dir=dir2" `shouldRespondWith` 200
        postHtmlForm "/files/new" [("new-file", "new")] `shouldRespondWith` 200
        liftIO do
          exists <- doesFileExist (root </> "dir2/new")
          exists `shouldBe` True


  describe "/files/update" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application env) do
      it "should update a file `new` in root directory" do
        postHtmlForm "/files/update" [("path", "a"), ("content", "123")] `shouldRespondWith` 200
        liftIO do
          content <- readFile (root </> "a")
          content `shouldBe` "123"


  describe "Prevent access without logging-in" $ do
    env <- runIO $ mkEnv
    with (pure $ Filehub.application (env { noLogin = False })) do
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


  describe "Login" $ do
    !env <- runIO $ mkEnv >>= \e -> do
      userDB <- runEff . runFileSystem $ createUserDB (LoginInfo1 [("paul", "123"), ("peter", "345")])
      pure $ e
        { noLogin = False
        , userDB = userDB
        }
    let f =  UrlFormEncoded.urlEncodeAsForm . toForm
    with (pure $ Filehub.application $ env) do
      it "Login succeed, should redirect to /" do
        request methodPost "/login" [ (hContentType, "application/x-www-form-urlencoded") ] (f $ LoginForm "peter" "345") >>= \res -> liftIO do
            simpleStatus res `shouldBe` status200
            simpleBody res `shouldBe` ""
            lookup "Hx-Redirect" (simpleHeaders res) `shouldBe` Just "/"

        request methodPost "/login" [(hContentType, "application/x-www-form-urlencoded") ] (f $ LoginForm "paul" "123") >>= \res -> liftIO do
            simpleStatus res `shouldBe` status200
            simpleBody res `shouldBe` ""
            lookup "Hx-Redirect" (simpleHeaders res) `shouldBe` Just "/"

      it "Login failed, return the login form again" do
        request methodPost "/login" [ (hContentType, "application/x-www-form-urlencoded") ] (f $ LoginForm "paul" "xxx") `shouldRespondWith` 200
        request methodPost "/login" [ (hContentType, "application/x-www-form-urlencoded") ] (f $ LoginForm "peter" "xxx") `shouldRespondWith` 200


mkEnv :: IO Env
mkEnv = do
  sessionPool <- runEff SessionPool.new
  logger <- nullLogger
  let env =
        Env
          { port = 0
          , theme = Dark
          , sessionPool = sessionPool
          , sessionDuration = secondsToNominalDiffTime (60 * 60)
          , targets =
            [ Target $ FileBackend
              { targetId = tid
              , targetName = Nothing
              , root = root
              }
            ]
          , readOnly = False
          , logger = logger
          , logLevel = LogTrace
          , userDB = UserDB mempty
          , noLogin = True
          }
  pure env


setup :: IO ()
setup = do
  createDirectoryIfMissing True root
  forM_ fullPaths $ \file -> do
    createDirectoryIfMissing True (takeDirectory file)
    writeFile file "test content"


teardown :: IO ()
teardown = do
  removePathForcibly root


root :: FilePath
root = "/tmp/filehub-test/"


tid :: TargetId
tid = TargetId $ fromJust . UUID.fromString $ "11111111-35ad-49bb-b118-8e8fc24abf80"


fullPaths :: [FilePath]
fullPaths = map (root </>) testFiles


testFiles :: [FilePath]
testFiles =
  [ "a"
  , "b"
  , "dir1/x"
  , "dir2/subdir/y"
  ]


allPathsExist :: FilePath -> [FilePath] -> IO Bool
allPathsExist root' paths = and <$> mapM (pathExists root') paths


pathExists :: FilePath -> FilePath -> IO Bool
pathExists root' p = do
  let path = root' </> p
  file <- doesFileExist path
  dir  <- doesDirectoryExist path
  pure (file || dir)


nullLogger :: IO Logger
nullLogger = mkLogger "" $ \_ -> pure ()


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
