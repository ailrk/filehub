module Filehub.Error
  ( FilehubError(..)
  , withServerError
  , toServerError
  )
  where

import Servant
    ( ToHttpApiData(..),
      ServerError(errBody))
import Lens.Micro.Platform ()
import Data.Text qualified as Text
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (runErrorNoCallStack, throwError, Error)
import Data.String (IsString(..))
import Servant.Server (err500, err400)


data FilehubError
  = FileExists
  | InvalidPath
  | InvalidDir
  | InvalidSession
  | InvalidSelection
  | InvalidMimeTypeForThumbnail
  | InternalError
  | TargetError
  | FailedToDecodeImage
  | FailedToEncodeImage
  | MimeTypeMissing
  | S3Error
  | SelectError
  | CopyError
  | PasteError
  | LoginFailed
  deriving Show


instance ToHttpApiData FilehubError where
  toUrlPiece FileExists = Text.pack $ show FileExists
  toUrlPiece InvalidPath = Text.pack $ show InvalidPath
  toUrlPiece InvalidDir = Text.pack $ show InvalidDir
  toUrlPiece InvalidSession = Text.pack $ show InvalidSession
  toUrlPiece InvalidSelection = Text.pack $ show InvalidSelection
  toUrlPiece InvalidMimeTypeForThumbnail = Text.pack $ show InvalidMimeTypeForThumbnail
  toUrlPiece InternalError = Text.pack $ show InternalError
  toUrlPiece TargetError = Text.pack $ show TargetError
  toUrlPiece FailedToDecodeImage = Text.pack $ show FailedToDecodeImage
  toUrlPiece FailedToEncodeImage = Text.pack $ show FailedToEncodeImage
  toUrlPiece MimeTypeMissing = Text.pack $ show MimeTypeMissing
  toUrlPiece S3Error = Text.pack $ show S3Error
  toUrlPiece SelectError = Text.pack $ show SelectError
  toUrlPiece CopyError = Text.pack $ show CopyError
  toUrlPiece PasteError = Text.pack $ show PasteError
  toUrlPiece LoginFailed = Text.pack $ show LoginFailed


withServerError :: (Error ServerError :> es) => Eff (Error FilehubError : es) b -> Eff es b
withServerError action = runErrorNoCallStack action >>= either (\err -> throwError $ toServerError err) pure


toServerError :: FilehubError -> ServerError
toServerError err =
  case err of
    FileExists -> err400 { errBody = fromString $ show err }
    InvalidPath -> err400 { errBody = fromString $ show err }
    InvalidDir -> err400 { errBody = fromString $ show err }
    InvalidSession -> err400 { errBody = fromString $ show err }
    InvalidSelection -> err400 { errBody = fromString $ show err }
    InvalidMimeTypeForThumbnail -> err400 { errBody = fromString $ show err }
    InternalError -> err500 { errBody = fromString $ show err }
    TargetError -> err500 { errBody = fromString $ show err }
    FailedToDecodeImage -> err500 { errBody = fromString $ show err }
    FailedToEncodeImage -> err500 { errBody = fromString $ show err }
    MimeTypeMissing -> err400 { errBody = fromString $ show err }
    S3Error -> err500 { errBody = fromString $ show err }
    SelectError -> err500 { errBody = fromString $ show err }
    CopyError -> err500 { errBody = fromString $ show err }
    PasteError -> err500 { errBody = fromString $ show err }
    LoginFailed -> err500 { errBody = fromString $ show err }
