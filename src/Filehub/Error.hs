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
  | InternalError
  | TargetError
  | MimeTypeMissing
  | S3Error
  deriving Show


instance ToHttpApiData FilehubError where
  toUrlPiece FileExists = Text.pack $ show FileExists
  toUrlPiece InvalidPath = Text.pack $ show InvalidPath
  toUrlPiece InvalidDir = Text.pack $ show InvalidDir
  toUrlPiece InvalidSession = Text.pack $ show InvalidSession
  toUrlPiece InternalError = Text.pack $ show InternalError
  toUrlPiece TargetError = Text.pack $ show TargetError
  toUrlPiece MimeTypeMissing = Text.pack $ show MimeTypeMissing
  toUrlPiece S3Error = Text.pack $ show S3Error


withServerError :: (Error ServerError :> es) => Eff (Error FilehubError : es) b -> Eff es b
withServerError action = runErrorNoCallStack action >>= either (\err -> throwError $ toServerError err) pure


toServerError :: FilehubError -> ServerError
toServerError err =
  case err of
    FileExists -> err400 { errBody = fromString $ show err }
    InvalidPath -> err400 { errBody = fromString $ show err }
    InvalidDir -> err400 { errBody = fromString $ show err }
    InvalidSession -> err400 { errBody = fromString $ show err }
    InternalError -> err500 { errBody = fromString $ show err }
    TargetError -> err500 { errBody = fromString $ show err }
    MimeTypeMissing -> err400 { errBody = fromString $ show err }
    S3Error -> err500 { errBody = fromString $ show err }
