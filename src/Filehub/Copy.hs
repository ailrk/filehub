{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Filehub.Copy
  ( select
  , copy
  , paste
  , getCopyState
  , setCopyState
  , clearCopyState
  , CopyState
  )
  where

import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Reader.Dynamic (Reader)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log, logAttention_, logAttention)
import Filehub.Types (Env, CopyState(..), SessionId, File(..), Selected (..))
import Filehub.Error (FilehubError (..))
import Filehub.Env (TargetView(..))
import Filehub.Env qualified as Env
import Filehub.Storage (runStorage)
import Filehub.Storage qualified as Storage
import Filehub.Target qualified as Target
import Filehub.Selected qualified as Selected
import Filehub.ClientPath qualified as ClientPath
import Control.Monad (forM_)
import System.FilePath (takeFileName, (</>))
import Data.String.Interpolate (i)
import Data.Function (on)
import Data.List (nub)


getCopyState :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es CopyState
getCopyState sessionId = (^. #copyState) <$> Env.getSession sessionId


setCopyState :: (Reader Env :> es, IOE :> es) => SessionId -> CopyState -> Eff es ()
setCopyState sessionId copyState = Env.updateSession sessionId $ \s -> s & #copyState .~ copyState


clearCopyState :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clearCopyState sessionId = setCopyState sessionId NoCopyPaste


-- | Add selected to copy state.
select :: (Reader Env :> es, IOE :> es, FileSystem :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es ()
select sessionId = do
  allSelecteds <- Selected.allSelecteds sessionId
  forM_ allSelecteds $ \(target, selected) -> do
    do
      let tid = Target.getTargetId target
      logAttention_ [i|#{tid}, #{selected}|]
    Target.withTarget sessionId (Target.getTargetId target) do
      case selected of
        NoSelection -> do
          state <- getCopyState sessionId
          case state of
            NoCopyPaste -> setCopyState sessionId (CopySelected [])
            CopySelected {} -> pure ()
            _ -> do
              logAttention_ [i|Select error: #{sessionId}|]
              throwError SelectError
        Selected x xs -> do
          root <- Env.getRoot sessionId
          let paths = (x:xs) & fmap (ClientPath.fromClientPath root)
          files <- traverse (runStorage sessionId . Storage.getFile) paths
          state <- getCopyState sessionId
          case state of
            NoCopyPaste -> setCopyState sessionId (CopySelected [(target, files)])
            CopySelected selections -> setCopyState sessionId (CopySelected (merge (target, files) selections))
            _ -> do
              logAttention_ [i|Select error: #{sessionId}|]
              throwError SelectError
  where
    merge sel [] = [sel]
    merge sel@(target, files) (h@(target', files'):rest)
      | on (==) Target.getTargetId target target' = (target, nub (files <> files')):rest
      | otherwise = h:merge sel rest


-- | Confirm selection
copy :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es ()
copy sessionId = do
  state <- getCopyState sessionId
  case state of
    CopySelected selections -> setCopyState sessionId (Paste selections)
    _ -> do
      logAttention_ [i|Copy error: #{sessionId}, not in copyable state.|]
      throwError CopyError


-- | Paste files
paste :: (Reader Env :> es, IOE :> es, FileSystem :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es ()
paste sessionId = do
  state <- getCopyState sessionId
  case state of
    Paste selections -> do
      TargetView to _ _ <- Env.currentTarget sessionId
      logAttention "SELECTIONS: "  (fmap (\(t, s) -> (show (Target.getTargetId t), show s)) selections)

      forM_ selections $ \(from, files) -> do
        forM_ files $ \file -> do
          bytes <- Target.withTarget sessionId (Target.getTargetId from) do
            runStorage sessionId $ Storage.readFileContent file
          Target.withTarget sessionId (Target.getTargetId to) do
            dirPath <- Env.getCurrentDir sessionId
            let destination = dirPath </> takeFileName file.path
            runStorage sessionId $ Storage.writeFile destination bytes
      setCopyState sessionId NoCopyPaste
      Selected.clearSelectedAllTargets sessionId
    _ -> do
      logAttention_ [i|Paste error: #{sessionId}, not in pastable state.|]
      throwError PasteError
