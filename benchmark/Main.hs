module Main where


import Criterion
import Criterion.Main (defaultMain)
import UnliftIO.Process


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
