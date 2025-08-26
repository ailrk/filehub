{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Filehub.Storage
  ( Storage(..)
  , getStorage
  )
  where


import Data.Generics.Labels ()
import Effectful ( Eff, Eff )
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logAttention_)
import Filehub.Target.File (FileSys)
import Filehub.Target.S3 (S3)
import Filehub.Target.Types (targetHandler)
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Session qualified as Session
import Filehub.Target (TargetView(..), handleTarget)
import Filehub.Storage.Context qualified as Storage
import Filehub.Storage.File qualified as File
import Filehub.Storage.S3 qualified as S3
import Filehub.Target.Types (Storage(..))
import Filehub.Types (SessionId)
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)


getStorage :: Storage.Context es => SessionId -> Eff es (Storage (Eff es))
getStorage sessionId = do
  TargetView target _ _ <- Session.currentTarget sessionId
  maybe onError pure $ handleTarget target
    [ targetHandler @FileSys $ \_ -> File.storage sessionId
    , targetHandler @S3 $ \_ -> S3.storage sessionId
    ]
  where
    onError = do
      logAttention_ "[getStorage] target error"
      throwError (FilehubError TargetError "Invalid target")
