{-# OPTIONS_GHC -Wno-type-defaults #-}
module Test.Api.File (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Filehub.Server qualified as Filehub
import System.Directory (createDirectoryIfMissing, removePathForcibly, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import System.FilePath (takeDirectory)
import Test.Hspec
import Test.Hspec.Wai
import Util.Env (root, defaultEnv)
import UnliftIO.Process (readProcess)


spec :: Spec
spec = before setup  . after_ teardown . with (Filehub.application <$> defaultEnv) $ do
  describe "/files/paste" do
    it "when not copied - should fail" do
      post "/files/paste" "" `shouldRespondWith` 500

    it "when not copied - should fail" do
      post "/files/paste" "" `shouldRespondWith` 500

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
      postHtmlForm "/table/select" [("selected", "dir1")] `shouldRespondWith` 200
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


setup :: IO ()
setup = do
  createDirectoryIfMissing True root
  forM_ fullPaths \file -> do
    createDirectoryIfMissing True (takeDirectory file)
    writeFile file "test content"


teardown :: IO ()
teardown = do
  removePathForcibly root


fullPaths :: [FilePath]
fullPaths = map (root </>) testFiles


allPathsExist :: FilePath -> [FilePath] -> IO Bool
allPathsExist root' paths = and <$> mapM (pathExists root') paths


pathExists :: FilePath -> FilePath -> IO Bool
pathExists root' p = do
  let path = root' </> p
  file <- doesFileExist path
  dir  <- doesDirectoryExist path
  pure (file || dir)


testFiles :: [FilePath]
testFiles =
  [ "a"
  , "b"
  , "dir1/x"
  , "dir2/subdir/y"
  ]


dumpDir :: FilePath -> IO String
dumpDir root' = do readProcess "tree" [root'] ""


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
