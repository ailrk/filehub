module Data.ClientPath.Effectful (validateAbsPath) where


import Effectful ((:>), Eff)
import Effectful.Error.Dynamic (Error, throwError)
import Data.ClientPath (AbsPath(..), newAbsPath)


validateAbsPath :: (Error e :> es) => FilePath -> e -> Eff es AbsPath
validateAbsPath path err = case newAbsPath path of
                             Just absPath -> pure absPath
                             Nothing -> throwError err
