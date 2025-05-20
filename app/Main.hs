module Main (main) where

import Filehub.Entry qualified
import GHC.Stack (whoCreated)
import Control.Exception (SomeException(..), catch, throwIO)


main :: IO ()
main = do
  Filehub.Entry.main `catch` \e@SomeException {} -> do -- neat trick to get stack trace.
    mapM_ putStrLn =<< whoCreated e
    throwIO e
