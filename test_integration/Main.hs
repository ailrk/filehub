{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
module Main where

import Cache.InMemory qualified
import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as ByteString
import Data.Maybe (fromJust)
import Data.Time (secondsToNominalDiffTime)
import Data.UUID qualified as UUID
import Effectful (runEff)
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (LogLevel(LogTrace), Logger)
import Filehub.ActiveUser.Pool qualified as ActiveUser.Pool
import Filehub.Auth.OIDC (OIDCAuthProviders(..))
import Filehub.Auth.Simple (UserRecord(..))
import Filehub.Auth.Simple (createSimpleAuthUserDB)
import Filehub.Env (Env(..))
import Filehub.Locale (Locale(..))
import Filehub.Server qualified as Filehub
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Types
    ( Theme(..), LoginForm(..) )
import LockRegistry.Local qualified
import Log (mkLogger, LogMessage(..))
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai.Test hiding (request)
import System.Directory (createDirectoryIfMissing, removePathForcibly, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import System.FilePath (takeDirectory)
import Target.File (TargetBackend(..))
import Target.Types (TargetId(..), AnyTarget(..))
import Test.Hspec
import Test.Hspec.Wai
import Web.FormUrlEncoded (ToForm(..))
import Web.FormUrlEncoded qualified as UrlFormEncoded
import Filehub.Auth.Simple (SimpleAuthUserDB(..))
import Effectful.Process (readProcess, runProcess)
import GHC.Conc (newTVarIO)
import Filehub.SharedLink qualified as SharedLink
import Effectful.Concurrent (runConcurrent)
import Test.Hspec.Runner
import Control.Concurrent (threadDelay)
import Data.Text qualified as Text
import EvtLog qualified
import Data.ClientPath (AbsPath(..))


main :: IO ()
main = hspecWith
  (defaultConfig
    { configPrettyPrint = True
    , configPrintCpuTime = True
    }
  ) do
  middlewareSpec
  apiSpec
  loginSpec


middlewareSpec :: Spec
middlewareSpec = before setup  . after_ teardown . with (Filehub.application <$> defaultEnv) $ do
  describe "Session middlware" $ do
      -- Request without session cookie should always get a new session id
      it "Should have Set-Cookie header with a new session id" do
        s <- get "/healthz"
        liftIO $ do
          simpleStatus s `shouldBe` status200
          case lookup hSetCookie $ simpleHeaders s of
            Just raw -> ByteString.unpack raw `shouldContain` "sessionId="
            Nothing -> expectationFailure ("No Set-Cookie header. " <> show (simpleHeaders s))


apiSpec :: Spec
apiSpec = before setup  . after_ teardown . with (Filehub.application <$> defaultEnv) $ do
  describe "/files/paste" do
    it "when not copied - should fail" do post "/files/paste" "" `shouldRespondWith` 500

    it "when not copied - should fail" do post "/files/paste" "" `shouldRespondWith` 500

    it "paste file into the same dir - should overwrite the file with the same content." do
      postHtmlForm "/table/select" [("selected", "a")] `shouldRespondWith` 200
      get "/files/copy" `shouldRespondWith` 200
      post "/files/paste" "" `shouldRespondWith` 200

    it "to a new dir - should succeed" do
      postHtmlForm "/table/select" [("selected", "a")] `shouldRespondWith` 200
      get "/files/copy" `shouldRespondWith` 200
      get "/cd?dir=dir1" `shouldRespondWith` 200
      post "/files/paste" "" `shouldRespondWith` 200

      liftIO do
        doesFileExist (root </> "dir1/a") `waitUntilTrueOr` do
          dirStructure <- dumpDir root
          expectationFailure dirStructure

    it "multiple files at the same level - should paste successfully" do
      postHtmlForm "/table/select"
        [ ("selected", "a")
        , ("selected", "b")
        , ("selected", "dir1")
        ] `shouldRespondWith` 200
      get "/files/copy" `shouldRespondWith` 200
      get "/cd?dir=dir2" `shouldRespondWith` 200
      post "/files/paste" "" `shouldRespondWith` 200
      liftIO do
        allPathsExist root ["dir2/a", "dir2/b", "dir2/dir1/x"] `waitUntilTrueOr` do
          dirStructure <- dumpDir root
          expectationFailure dirStructure

    it "paste a dir - dir1 should be pasted into dir2 completely" do
      postHtmlForm "/table/select" [("selected", "/dir1")] `shouldRespondWith` 200
      get "/files/copy" `shouldRespondWith` 200
      get "/cd?dir=dir2" `shouldRespondWith` 200
      post "/files/paste" "" `shouldRespondWith` 200
      liftIO do
        allPathsExist root ["dir2/dir1/x"] `waitUntilTrueOr` do
          dirStructure <- dumpDir root
          expectationFailure dirStructure

  describe "/files/delete" do
    it "single file - file a should be deleted" do
      delete "/files/delete?file=a" `shouldRespondWith` 200
      liftIO do
        (not <$> doesFileExist (root </> "a")) `waitUntilTrueOr` do
          dirStructure <- dumpDir root
          expectationFailure dirStructure

        allPathsExist root ["b", "dir1/x", "dir2/subdir/y"] `waitUntilTrueOr` do
          dirStructure <- dumpDir root
          expectationFailure dirStructure

    it "a folder - directory dir2 sould be deleted" do
      delete "/files/delete?file=dir2" `shouldRespondWith` 200
      liftIO do
        (not <$> doesDirectoryExist (root </> "dir2")) `waitUntilTrueOr` do
          expectationFailure "Failed to copy"

        allPathsExist root ["a", "b", "dir1/x"] `waitUntilTrueOr` do
          dirStructure <- dumpDir root
          expectationFailure dirStructure

  describe "/files/new" do
    it "should create a file `new` in root directory" do
      postHtmlForm "/files/new" [("new-file", "new")] `shouldRespondWith` 200
      liftIO do
        doesFileExist (root </> "new") `waitUntilTrueOr` do
          dirStructure <- dumpDir root
          expectationFailure dirStructure

    it "should create a file `new` in dir2/" do
      get "/cd?dir=dir2" `shouldRespondWith` 200
      postHtmlForm "/files/new" [("new-file", "new")] `shouldRespondWith` 200
      liftIO do
        doesFileExist (root </> "dir2/new") `waitUntilTrueOr` do
          dirStructure <- dumpDir root
          expectationFailure dirStructure

  describe "/files/update" do
    it "should update a file `new` in root directory" do
      postHtmlForm "/files/update" [("path", "a"), ("content", "777")] `shouldRespondWith` 200
      liftIO do
        (readFile (root </> "a") >>= \c -> pure (c, c == "777"))
            `waitUntilTrueOr_`
            \content -> expectationFailure content


loginSpec :: Spec
loginSpec = before setup  . after_ teardown . with (Filehub.application <$> patchedEnv) $ do
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
      userDB <- runEff . runFileSystem $ createSimpleAuthUserDB [UserRecord "paul" "123", UserRecord "peter" "345"]
      pure
        env { simpleAuthUserDB = userDB }


defaultEnv :: IO Env
defaultEnv = do
  sessionPool    <- runEff Session.Pool.new
  activeUserPool <- runEff ActiveUser.Pool.new
  logger         <- nullLogger
  httpManager    <- newTlsManager
  cache          <- liftIO (Cache.InMemory.new 1000)
  lockRegistry   <- liftIO LockRegistry.Local.new
  targets        <- newTVarIO [ ( tid
                                , Target $ FileBackend
                                  { targetId = tid
                                  , targetName = Nothing
                                  , root = AbsPath root
                                  }
                                )
                              ]
  sharedLinkPool <- runEff . runConcurrent $ SharedLink.newShareLinkPool
  evtLogHandle   <- runEff $ EvtLog.initialize ":memory:" 100
  let env =
        Env
          { port = 0
          , theme = Dark
          , sessionPool = sessionPool
          , sessionDuration = secondsToNominalDiffTime (60 * 60)
          , sharedLinkPool = sharedLinkPool
          , targets = targets
          , readOnly = False
          , locale = EN
          , logger = logger
          , logLevel = LogTrace
          , enableWAILog = False
          , customThemeDark = Nothing
          , customThemeLight = Nothing
          , simpleAuthUserDB = SimpleAuthUserDB mempty
          , oidcAuthProviders = OIDCAuthProviders mempty
          , httpManager = httpManager
          , cache = cache
          , lockRegistry = lockRegistry
          , activeUsers = activeUserPool
          , evtLogHandle = evtLogHandle
          }
  pure env


setup :: IO ()
setup = do
  createDirectoryIfMissing True root
  forM_ fullPaths \file -> do
    createDirectoryIfMissing True (takeDirectory file)
    writeFile file "test content"


teardown :: IO ()
teardown = do
  removePathForcibly root


----------------------------------------
-- utils
----------------------------------------


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



waitUntilTrueOr :: IO Bool -> IO () -> IO ()
waitUntilTrueOr check action = go 50  -- 50 tries = ~5s max
  where
    go 0 = action
    go n = do
      ok <- check
      if ok then pure ()
            else do
              threadDelay 100000
              go (n - 1)


waitUntilTrueOr_ :: IO (a, Bool) -> (a -> IO ()) -> IO ()
waitUntilTrueOr_ check action = go (error "impossible") 50
  where
    go a 0 = action a
    go _ n = do
      (a', ok) <- check
      if ok then pure ()
            else do
              threadDelay 100000
              go a' (n - 1)


allPathsExist :: FilePath -> [FilePath] -> IO Bool
allPathsExist root' paths = and <$> mapM (pathExists root') paths


dumpDir :: FilePath -> IO String
dumpDir root' = do
  runEff . runProcess $ readProcess "tree" [root'] ""


pathExists :: FilePath -> FilePath -> IO Bool
pathExists root' p = do
  let path = root' </> p
  file <- doesFileExist path
  dir  <- doesDirectoryExist path
  pure (file || dir)


nullLogger :: IO Logger
nullLogger = mkLogger "" $ \msg -> do
  putStrLn (Text.unpack $ "    " <> msg.lmMessage)
  pure ()
