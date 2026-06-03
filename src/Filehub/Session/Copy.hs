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
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Monad (Filehub)
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (Selected(..), SessionId, CopyState (..))
import Target.Types qualified as Target
import Filehub.Session.Pool (withSession, modifySession, withSession_)
import Filehub.Session (Session(..), getTargetViews, withTarget, getStorage, getRoot)
import Control.Monad.Reader (MonadReader(ask))
import Control.Concurrent.STM (throwSTM)


clearCopyState :: SessionId -> Filehub ()
clearCopyState sessionId = modifySession sessionId \s -> pure s { copyState = NoCopyPaste }


-- | Add selected to copy state.
select :: SessionId -> Filehub ()
select sessionId = do
  env <- ask
  AllSelected { allSelected } <- withSession sessionId \s -> do
    targetViews <- getTargetViews env s
    pure $ Selected.getAllSelected targetViews

  forM_ allSelected \(target, selected) -> do
    withTarget sessionId target do
      paths <- withSession_ sessionId \s -> do
        case selected of
          NoSelection -> do
            case onNoSelection s.copyState of
              Right (Just state') -> pure (s { copyState = state' }, mempty)
              Right Nothing       -> pure (s, mempty)
              Left err -> do
                throwSTM err
          Selected x xs -> do
            root <- getRoot env s
            pure (s, (x:xs) & fmap (ClientPath.fromClientPath root))

      files <- do
        storage <- getStorage sessionId
        traverse storage.get paths

      withSession_ sessionId \s -> do
        case onSelected (target, files) s.copyState of
          Right state' -> pure (s { copyState = state' }, ())
          Left err -> do
            throwSTM err
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
copy sessionId = withSession_ sessionId \s -> do
  case step s.copyState of
    Right state' -> do
      pure ( s { copyState = state' }
           , ())
    Left err -> do
      throwSTM err
  where
    step = \case
      CopySelected selections -> Right (Paste selections)
      _                       -> Left (FilehubError SelectError "Not in a copyable state")
