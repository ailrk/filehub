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
import Filehub.Env qualified as Env
import Filehub.Target (TargetView(..))
import Filehub.Storage.Context qualified as Storage
import Filehub.Storage.File qualified as File
import Filehub.Storage.S3 qualified as S3
import Filehub.Storage.Types (Storage(..))
import Filehub.Types (SessionId, Target(..))
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)


getStorage :: Storage.Context es => SessionId -> Eff es (Storage (Eff es))
getStorage sessionId = do
  TargetView target _ _ <- Env.currentTarget sessionId
  undefined
  -- case target of
  --   undefined
    -- S3Target _ -> pure $ S3.storage sessionId
    -- FileTarget _ -> pure $ File.storage sessionId
