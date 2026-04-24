{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import Test.Api.File qualified
import Test.Api.Login qualified
import Test.Cache qualified
import Test.Hspec.Runner
import Test.Middleware qualified


main :: IO ()
main = hspecWith
  (defaultConfig
    { configPrettyPrint = True
    , configPrintCpuTime = True
    }
  ) do
    Test.Api.File.spec
    Test.Api.Login.spec
    Test.Middleware.spec
    Test.Cache.spec
