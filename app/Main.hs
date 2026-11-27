module Main (main) where

import Filehub.Prelude
import Filehub qualified
import GHC.Stack (whoCreated)
import Control.Exception (SomeException(..))


main :: IO ()
main = do
  Filehub.main `catch` \e@SomeException {} -> do -- neat trick to get stack trace.
    mapM_ putStrLn =<< whoCreated e
    throwIO e
