module Main where


import Wrk qualified
import Criterion
import Criterion.Main (defaultMain)
import System.Process


main :: IO ()
main = do
  (_, _, _, ph) <- createProcess (proc "cabal run filehub" [])
  defaultMain
    [ bgroup "server"
      [
      ]
    ]
  terminateProcess ph
  _ <- waitForProcess ph
  pure ()
