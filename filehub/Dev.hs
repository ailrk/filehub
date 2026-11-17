module Dev where

import Data.IORef
import Filehub qualified
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Async

{-# NOINLINE serverRef #-}
serverRef :: IORef (Maybe (Async ()))
serverRef = unsafePerformIO (newIORef Nothing)


reload :: String -> IO ()
reload args = do
  hSetBuffering stdout NoBuffering

  --  Tear down and release
  maybeOld <- readIORef serverRef
  case maybeOld of
    Just a -> do
      putStrLn "\n🔄 Stopping old server..."
      cancel a          -- Clean up brackets
      waitCatch a       -- Blocks briefly until the port is fully released
      putStrLn "✅ Old server stopped."
    Nothing -> return ()

  -- Start the fresh server
  putStrLn "> Starting server..."
  newAsync <- async (Filehub.mainDev args)
  writeIORef serverRef (Just newAsync)
  putStrLn "> Server running!"
