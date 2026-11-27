module Data.ClientPath.IO (validateAbsPath) where


import Control.Monad.IO.Class (MonadIO)
import Data.ClientPath (AbsPath(..))


-- TODO !THIS DOESN'T WORK WITH THE S3 PATH. WE NEED A MORE GENERAL  AbsPath TYPE.
-- CURRENT IT IS AN NOOP IT SIMMPLY WRAP THE PATH INTO THE NEWTYPE
validateAbsPath :: MonadIO m => FilePath -> e -> m AbsPath
validateAbsPath path err = pure (AbsPath path)
-- validateAbsPath path err = case newAbsPath path of
--                              Just absPath -> pure absPath
--                              Nothing -> throwError err
