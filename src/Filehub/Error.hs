module Filehub.Error
  ( FilehubError(..)
  , Error'(..)
  , withServerError
  , toServerError
  )
  where

import Servant
    ( ServerError(errBody))
import Lens.Micro.Platform ()
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (runErrorNoCallStack, throwError, Error)
import Data.String (IsString(..))
import Servant.Server (err500, err400)

data FilehubError = FilehubError Error' String deriving (Show, Eq)

data Error'
  = FileExists
  | InvalidPath
  | InvalidDir
  | InvalidSession
  | InvalidSelection
  | FormatError
  | InternalError
  | TargetError
  | LocaleError
  | FailedToDecodeImage
  | FailedToEncodeImage
  | MimeTypeMissing
  | S3Error
  | SelectError
  | CopyError
  | PasteError
  | LoginFailed
  deriving (Show, Eq)


withServerError :: (Error ServerError :> es) => Eff (Error FilehubError : es) b -> Eff es b
withServerError action = runErrorNoCallStack action >>= either (\err -> throwError $ toServerError err) pure


toServerError :: FilehubError -> ServerError
toServerError (FilehubError err msg) =
  case err of
    FileExists ->          err400 { errBody = fromString msg }
    InvalidPath ->         err400 { errBody = fromString msg }
    InvalidDir ->          err400 { errBody = fromString msg }
    InvalidSession ->      err400 { errBody = fromString msg }
    InvalidSelection ->    err400 { errBody = fromString msg }
    FormatError ->         err400 { errBody = fromString msg }
    InternalError ->       err500 { errBody = fromString msg }
    TargetError ->         err500 { errBody = fromString msg }
    LocaleError ->         err400 { errBody = fromString msg }
    FailedToDecodeImage -> err500 { errBody = fromString msg }
    FailedToEncodeImage -> err500 { errBody = fromString msg }
    MimeTypeMissing ->     err400 { errBody = fromString msg }
    S3Error ->             err500 { errBody = fromString msg }
    SelectError ->         err500 { errBody = fromString msg }
    CopyError ->           err500 { errBody = fromString msg }
    PasteError ->          err500 { errBody = fromString msg }
    LoginFailed ->         err500 { errBody = fromString msg }
