module Data.ClientPath.Effectful (validateAbsPath) where


import Effectful ((:>), Eff)
import Effectful.Error.Dynamic (Error, throwError)
import Data.ClientPath (AbsPath(..), newAbsPath)


-- TODO !THIS DOESN'T WORK WITH THE S3 PATH. WE NEED A MORE GENERAL  AbsPath TYPE.
-- CURRENT IT IS AN NOOP IT SIMMPLY WRAP THE PATH INTO THE NEWTYPE
validateAbsPath :: FilePath -> e -> Eff es AbsPath
validateAbsPath path err = pure (AbsPath path)
-- validateAbsPath path err = case newAbsPath path of
--                              Just absPath -> pure absPath
--                              Nothing -> throwError err
