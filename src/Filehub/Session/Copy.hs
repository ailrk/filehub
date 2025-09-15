module Filehub.Session.Copy
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
import Effectful.Log (Log, logAttention_)
import Filehub.Types (Env, CopyState(..), SessionId, File(..), Selected (..))
import Filehub.Target (TargetView(..))
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Session qualified as Session
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Storage (getStorage, Storage(..))
import Filehub.Target qualified as Target
import Filehub.Session.Selected qualified as Selected
import Filehub.ClientPath qualified as ClientPath
import Control.Monad (forM_)
import System.FilePath (takeFileName, (</>))
import Data.String.Interpolate (i)
import Data.Function (on)
import Data.List (nub)
import Filehub.Effectful.Cache (Cache)
import Filehub.Effectful.LockManager (LockManager)


getCopyState :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es CopyState
getCopyState sessionId = (^. #copyState) <$> Session.Pool.get sessionId


setCopyState :: (Reader Env :> es, IOE :> es) => SessionId -> CopyState -> Eff es ()
setCopyState sessionId copyState = Session.Pool.update sessionId \s -> s & #copyState .~ copyState


clearCopyState :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clearCopyState sessionId = setCopyState sessionId NoCopyPaste


-- | Add selected to copy state.
select :: (Reader Env :> es, IOE :> es, FileSystem :> es, Log :> es, Cache :> es, LockManager :> es, Error FilehubError :> es) => SessionId -> Eff es ()
select sessionId = do
  allSelecteds <- Selected.allSelecteds sessionId
  forM_ allSelecteds \(target, selected) -> do
    do
      let tid = Target.getTargetId target
      logAttention_ [i|#{tid}, #{selected}|]
    Session.withTarget sessionId (Target.getTargetId target) \_ -> do
      case selected of
        NoSelection -> do
          state <- getCopyState sessionId
          case onNoSelection state of
            Right (Just state') -> setCopyState sessionId state'
            Right Nothing       -> pure ()
            Left err -> do
              logAttention_ [i|#{err}|]
              throwError err
        Selected x xs -> do
          root      <- Session.getRoot sessionId
          storage   <- getStorage sessionId
          let paths =  (x:xs) & fmap (ClientPath.fromClientPath root)
          files     <- traverse storage.get paths
          state     <- getCopyState sessionId
          case onSelected (target, files) state of
            Right state' -> setCopyState sessionId state'
            Left err -> do
              logAttention_ [i|#{err}|]
              throwError err
  where
    merge sel [] = [sel]
    merge sel@(target, files) (h@(target', files'):rest)
      | on (==) Target.getTargetId target target' = (target, nub (files <> files')):rest
      | otherwise = h:merge sel rest

    onNoSelection = \case
      NoCopyPaste    -> Right . Just $ CopySelected []
      CopySelected _ -> Right Nothing
      Paste _        -> Left (FilehubError SelectError "Invalid selection")

    onSelected (target, files) = \case
      NoCopyPaste             -> Right (CopySelected [(target, files)])
      CopySelected selections -> Right (CopySelected (merge (target, files) selections))
      Paste _                 -> Left (FilehubError SelectError "Invalid selection")


-- | Confirm selection
copy :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es ()
copy sessionId = do
  state <- getCopyState sessionId
  case step state of
    Right state' -> setCopyState sessionId state'
    Left err -> do
      logAttention_ [i|#{err}|]
      throwError err
  where
    step = \case
      CopySelected selections -> Right (Paste selections)
      _                       -> Left (FilehubError SelectError "Not in a copyable state")


-- | Paste files
paste :: (Reader Env :> es, IOE :> es, FileSystem :> es, Log :> es, Cache :> es, LockManager :> es, Error FilehubError :> es) => SessionId -> Eff es ()
paste sessionId = do
  state <- getCopyState sessionId
  case state of
    Paste selections -> do
      TargetView to _ _ <- Session.currentTarget sessionId
      forM_ selections \(from, files) -> do
        forM_ files \file -> do
          bytes <- Session.withTarget sessionId (Target.getTargetId from) \_ -> do
            storage <- getStorage sessionId
            storage.read file
          Session.withTarget sessionId (Target.getTargetId to) \_ -> do
            storage         <- getStorage sessionId
            dirPath         <- Session.getCurrentDir sessionId
            let destination =  dirPath </> takeFileName file.path
            storage.write destination bytes
      setCopyState sessionId NoCopyPaste
      Selected.clearSelectedAllTargets sessionId
    _ -> do
      logAttention_ [i|Paste error: #{sessionId}, not in pastable state.|]
      throwError (FilehubError SelectError "Not in a pastable state")
