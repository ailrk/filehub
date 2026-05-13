{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Session.Copy
  ( select
  , copy
  , clearCopyState
  , CopyState
  )
  where

import Control.Handle.Storage (Storage(..))
import Control.Monad (forM_)
import Data.ClientPath qualified as ClientPath
import Data.Function (on, (&))
import Data.List (nub)
import Data.String.Interpolate (i)
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Monad (Filehub)
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (Selected(..), SessionId, CopyState (..), SessionGet(..), SessionSet(..))
import Log (logAttention_)
import Target.Types qualified as Target
import UnliftIO (throwIO)
import {-# SOURCE #-} Filehub.Session.Handle (withTarget, get, set)


clearCopyState :: SessionId -> Filehub ()
clearCopyState sessionId = set sessionId (.copyState) NoCopyPaste


-- | Add selected to copy state.
select :: SessionId -> Filehub ()
select sessionId = do
  AllSelected { allSelected } <- Selected.getAllSelected sessionId
  forM_ allSelected \(target, selected) -> do
    withTarget sessionId target do
      storage <- get sessionId (.storage)
      case selected of
        NoSelection -> do
          state <- get sessionId (.copyState)
          case onNoSelection state of
            Right (Just state') -> set sessionId (.copyState) state'
            Right Nothing       -> pure ()
            Left err -> do
              logAttention_ [i|[asckkk] #{err}|]
              throwIO err
        Selected x xs -> do
          root <- get sessionId (.root)
          let paths = (x:xs) & fmap (ClientPath.fromClientPath root)
          files <- traverse storage.get paths
          state <- get sessionId (.copyState)
          case onSelected (target, files) state of
            Right state' -> set sessionId (.copyState) state'
            Left err -> do
              logAttention_ [i|[ascks1] #{err}|]
              throwIO err
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
copy :: SessionId -> Filehub ()
copy sessionId = do
  state <- get sessionId (.copyState)
  case step state of
    Right state' -> do
      set sessionId (.copyState) state'
    Left err -> do
      logAttention_ [i|[tyy33d] #{err}|]
      throwIO err
  where
    step = \case
      CopySelected selections -> Right (Paste selections)
      _                       -> Left (FilehubError SelectError "Not in a copyable state")
