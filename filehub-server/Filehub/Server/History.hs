{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Filehub.Server.History (history) where

import Filehub.Prelude
import Control.Monad.Reader (asks)
import Data.ClientPath (Root(..), AbsPath(..))
import Filehub.Env (Env(..))
import Filehub.Handler (ConfirmLogin)
import Filehub.History (getHistory)
import Filehub.Monad
import Filehub.Locale (phrase)
import Filehub.Session (SessionId, Session(..))
import Filehub.Session.Pool (withSession)
import Filehub.Template.History (historyModal)
import Lucid (Html)
import System.FilePath (takeFileName)
import Target.File (FileSys)
import Target.File qualified as TFile
import Target.S3 (S3)
import Target.S3 qualified as TS3
import Target.Types (AnyTarget, TargetHandler(..), handleTarget, Target)


targetLabel :: AnyTarget -> Text
targetLabel t = fromMaybe "?" $ handleTarget t
  [ TargetHandler \(TFile.FileBackend { targetName = tn, root = Root (AbsPath p) } :: Target FileSys) ->
      fromMaybe (txt_pack (takeFileName p)) tn
  , TargetHandler \(TS3.S3Backend { bucket = b } :: Target S3) -> b
  ]


history :: SessionId -> ConfirmLogin -> Filehub (Html ())
history sessionId _ = do
  h       <- asks (.history)
  targets <- liftIO . readTVarIO =<< asks (.targets)
  locale  <- withSession sessionId \(Session { locale = loc }) -> pure loc
  entries <- liftIO (getHistory h)
  let labelOf tid = fromMaybe "?" (lookup tid targets >>= \t -> Just (targetLabel t))
  pure (historyModal (phrase locale) labelOf entries)
